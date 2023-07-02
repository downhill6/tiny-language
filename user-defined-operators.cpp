#include "./KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// 定义词法分析器中不同类型的 Token
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,

    // control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,

    // operators
    tok_binary = -11,
    tok_unary = -12
};

// 全局变量，用于保存当前识别的 token
static std::string IdentifierStr;
// 全局变量，用于保存当前识别的数字
static double NumVal;

/// 从标准输入中获取下一个 token
static int gettok() {
    static int LastChar = ' ';

    // 跳过空白字符
    while (isspace(LastChar))
        LastChar = getchar();

    // 标识符: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(LastChar)) { 
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "then")
            return tok_then;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "for")
            return tok_for;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "binary")
            return tok_binary;
        if (IdentifierStr == "unary")
            return tok_unary;
        return tok_identifier;
    }

    // 数字: [0-9.]+
    if (isdigit(LastChar) || LastChar == '.') {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    // 检查文件结束符，返回 tok_eof
    if (LastChar == EOF)
        return tok_eof;

    // 否则，只返回字符作为其ascii值。
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {
    // 在每个AST类中添加虚拟代码生成方法(codegen)

    /// ExprAST - 所有表达式节点的基类
    class ExprAST {
    public:
        virtual ~ExprAST() = default;
        // Value 是 LLVM 中用于表示“静态单赋值（SSA）寄存器”或“SSA 值”的类。
        // https://en.wikipedia.org/wiki/Static_single-assignment_form
        virtual Value *codegen() = 0;
    };

    /// NumberExprAST - 数字字面量表达式节点，例如 "1.0"
    class NumberExprAST : public ExprAST {
        double Val;

    public:
        NumberExprAST(double Val) : Val(Val) {}

        Value *codegen() override;
    };

    /// VariableExprAST - 变量引用表达式节点，例如 "a"
    class VariableExprAST : public ExprAST {
        std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}

        Value *codegen() override;
    };

    /// UnaryExprAST - 一元表达式节
    class UnaryExprAST : public ExprAST {
        char Opcode;
        std::unique_ptr<ExprAST> Operand;

    public:
        UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
            : Opcode(Opcode), Operand(std::move(Operand)) {}

        Value *codegen() override;
    };

    /// BinaryExprAST - 二元表达式节点，例如 "a + b" 或 "a - b"
    class BinaryExprAST : public ExprAST {
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                      std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value *codegen() override;
    };

    /// CallExprAST 表示函数调用表达式，包括被调用的函数名和参数列表等信息。
    /// 在编译器或解释器中，函数调用用于执行函数的逻辑，将参数传递给函数并返回函数的计算结果。
    class CallExprAST : public ExprAST {
        // Callee: 函数名，Args: 参数列表
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string &Callee,
                    std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}

        Value *codegen() override;
    };

    /// IfExprAST - if/then/else 表达式节点，包括条件表达式、then 分支和 else 分支等信息。
    /// if/then/else 用于实现条件分支，根据条件表达式的计算结果来决定执行哪个分支。
    class IfExprAST : public ExprAST {
        std::unique_ptr<ExprAST> Cond, Then, Else;

    public:
        IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
                    std::unique_ptr<ExprAST> Else)
            : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

        Value *codegen() override;
    };

    /// ForExprAST 表示 for/in 循环表达式，包括循环变量、起始值、结束值、步长和循环体等信息。
    class ForExprAST : public ExprAST {
        // VarName: 循环变量名，Start: 起始值，End: 结束值，Step: 步长，Body: 循环体
        std::string VarName;
        std::unique_ptr<ExprAST> Start, End, Step, Body;

    public:
        ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
                    std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
                    std::unique_ptr<ExprAST> Body)
            : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
                Step(std::move(Step)), Body(std::move(Body)) {}

        Value *codegen() override;
    };

    /// PrototypeAST 表示函数的声明，用于描述函数的基本信息，如函数名和参数等，但不包含函数体。
    /// 在编译器中，函数原型通常用于类型检查和链接过程，以便在代码中引用该函数时能够正确地进行类型匹配和链接。
    /// 在解释器中，函数原型可以用于动态加载函数，实现类似于动态链接库的功能。
    class PrototypeAST {
        std::string Name;
        std::vector<std::string> Args;
        // 用户自定义的操作符，当做函数来处理
        bool IsOperator;
        // 操作符的优先级
        unsigned Precedence;

    public:
        PrototypeAST(const std::string &Name, std::vector<std::string> Args,
                    bool IsOperator = false, unsigned Prec = 0)
            : Name(Name), Args(std::move(Args)), IsOperator(IsOperator), Precedence(Prec) {}

        Function *codegen();
        const std::string &getName() const { return Name; }

        bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
        bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

        char getOperatorName() const {
            assert(isUnaryOp() || isBinaryOp());
            // 函数名为 FnNameOp，获取函数名的最后一个字符，也就是实际定义的操作符
            return Name[Name.size() - 1];
        }

        unsigned getBinaryPrecedence() const { return Precedence; }
    };

    /// FunctionAST 表示函数的定义，包括函数的原型和函数体。
    /// 在编译器中，函数定义用于生成目标代码，将函数的逻辑转换为可执行代码。
    /// 在解释器中，函数定义用于执行函数的逻辑，实现函数调用的功能。
    class FunctionAST {
        std::unique_ptr<PrototypeAST> Proto;
        std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                    std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}

        Function *codegen();
    };

}
// end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//


/// CurTok/getNextToken - 提供一个简单的 token 缓冲区。
/// CurTok 是解析器正在查看的当前 token。
/// getNextToken 从词法分析器中读取下一个 token，并将其结果更新 CurTok。
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - 保存定义的每个二元操作符的优先级
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - 获取当前二元操作符的优先级
static int GetTokPrecedence() {
    if (!isascii(CurTok))
        return -1;

    // 确保当前 token 是一个二元操作符
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

/// LogError* - 这些是用于错误处理的辅助函数。
std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}

// ParseExpression - 解析表达式 前置声明
static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
/// 解析数字字面量表达式
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    // 识别完数字后，需要从词法分析器中读取下一个 token，以便继续解析后面的内容。
    getNextToken();
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
/// 解析括号表达式
static std::unique_ptr<ExprAST> ParseParenExpr() {
    // 消耗掉 '('
    getNextToken(); 
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')')
        return LogError("expected ')'");
    // 消耗掉 ')'
    getNextToken();
    return V;
}


/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
/// 解析标识符表达式
/// 识别标识符表达式时，需要查看下一个 token 是否是 '('，如果是，则说明当前标识符是一个函数调用。
/// 如果不是，则说明当前标识符是一个变量引用。
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    // 保存当前标识符的名称
    std::string IdName = IdentifierStr;

    // 消耗掉标识符
    getNextToken();

    // 当前标识符是一个变量引用
    if (CurTok != '(')
        return std::make_unique<VariableExprAST>(IdName);

    // 当前标识符是一个函数调用
    getNextToken(); //消耗掉 "("
    std::vector<std::unique_ptr<ExprAST>> Args;
    // 循环解析参数表达式
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')')
                break;

            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }

    // 消耗掉 ")"
    getNextToken();

    // 生成函数调用表达式
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
/// 解析 if/then/else 表达式
static std::unique_ptr<ExprAST> ParseIfExpr() {
    // 消耗掉 "if" 
    getNextToken();

    // 获取条件表达式
    auto Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    if (CurTok != tok_then)
        return LogError("expected then");
    // 消耗掉 "then"
    getNextToken();

    // 获取 then 分支表达式
    auto Then = ParseExpression();
    if (!Then)
        return nullptr;

    if (CurTok != tok_else)
        return LogError("expected else");

    // 消耗掉 "else"
    getNextToken();

    // 获取 else 分支表达式
    auto Else = ParseExpression();
    if (!Else)
        return nullptr;

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                      std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
/// 解析 for/in 循环
static std::unique_ptr<ExprAST> ParseForExpr() {
    // 消耗掉 "for".
    getNextToken();

    if (CurTok != tok_identifier)
        return LogError("expected identifier after for");

    // 保存循环变量的名称
    std::string IdName = IdentifierStr;
    // 消耗掉循环变量的名称
    getNextToken();

    if (CurTok != '=')
        return LogError("expected '=' after for");
    // 消耗掉 '='.
    getNextToken();

    // 解析循环变量的初始值
    auto Start = ParseExpression();
    if (!Start)
        return nullptr;
    if (CurTok != ',')
        return LogError("expected ',' after for start value");
    getNextToken();

    // 解析循环变量的结束值
    auto End = ParseExpression();
    if (!End)
        return nullptr;

    // The step value is optional.
    // 解析循环变量的步长，如果有 "," 说明存在步长
    // 如果没有 "," 说明不存在步长，步长默认为 1
    std::unique_ptr<ExprAST> Step;
    if (CurTok == ',') {
        getNextToken();
        Step = ParseExpression();
        if (!Step)
        return nullptr;
    }

    if (CurTok != tok_in)
        return LogError("expected 'in' after for");
    // 消耗掉 "in".
    getNextToken();

    // 解析 for/in 循环体
    auto Body = ParseExpression();
    if (!Body)
        return nullptr;

    return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                        std::move(Step), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
/// 解析基础表达式 标识符表达式、数字字面量、括号表达式
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
    default:
        return LogError("unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_if:
        return ParseIfExpr();
    case tok_for:
        return ParseForExpr();
    }
}

/// unary
///   ::= primary
///   ::= '!' unary
/// 解析一元表达式
static std::unique_ptr<ExprAST> ParseUnary() {
    // 如果当前 token 不是一个操作符，则它是一个基础表达式
    if (!isascii(CurTok) || CurTok == '(' || CurTok == ',') {
        return ParsePrimary();
    }

    // 如果当前 token 是一个一元操作符，则解析一元表达式
    int Opc = CurTok;
    getNextToken();
    if (auto Operand = ParseUnary())
        return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
    return nullptr;
}

/// binoprhs
///   ::= ('+' unary)*
/// 递归解析二元表达式右侧
static std::unique_ptr<ExprAST> ParseBinOpRHS(
    // ExprPrec 表示上一个二元操作符的优先级，LHS 表示当前表达式的左侧部分
    int ExprPrec, 
    std::unique_ptr<ExprAST> LHS
) {
    // 如果当前 token 是一个二元操作符，则获取其优先级
    while (true) {
        int TokPrec = GetTokPrecedence();

        // 当前二元操作符的优先级小于前一个二元操作符的优先级
        // 循环在此处结束
        if (TokPrec < ExprPrec)
            return LHS;

        // 保存当前二元操作符
        int BinOp = CurTok;
        // 获取下一个 token
        getNextToken();

        // 解析当前的 token, 也就是二元操作符右侧的表达式
        auto RHS = ParseUnary();

        if (!RHS)
            return nullptr;

        // 如果当前二元操作符的优先级小于下一个二元操作符的优先级，则需要递归解析下一个二元操作符右侧的表达式
        // 例如：a + b * c，当解析到 "+" 时，发现其优先级小于 "*"，则需要递归解析 "*" 右侧的表达式
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // 合并 LHS 和 RHS，例如：a + b + c，当解析到第二个 "+" 时，需要将其与之前解析的 "a + b" 合并
        // 生成 BinaryExprAST 节点，将其保存到 LHS 中，继续循环解析下一个二元操作符
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= unary binoprhs
/// 解析表达式
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParseUnary();
    if (!LHS)
        return nullptr;

    // 传入初始优先级 0，递归的解析二元表达式右侧
    return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= identifier '(' identifier* ')'
///   ::= binary LETTER number? (id, id)
///   ::= unary LETTER (id)
/// 解析函数原型和操作符原型
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    std::string FnName;

    // 0 = identifier, 1 = unary, 2 = binary.
    unsigned Kind = 0;
    unsigned BinaryPrecedence = 30;

    switch (CurTok) {
    case tok_identifier:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;
    case tok_unary:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected unary operator");
        FnName = "unary";
        // 把自定义的操作符作为函数名的一部分
        FnName += (char)CurTok;
        Kind = 1;
        getNextToken();
        break;
    case tok_binary:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected binary operator");
        FnName = "binary";
        FnName += (char)CurTok;
        Kind = 2;
        getNextToken();

        // 如果操作符定义中包含优先级，则获取优先级
        if (CurTok == tok_number) {
            if (NumVal < 1 || NumVal > 100)
                return LogErrorP("Invalid precedence: must be 1..100");
            BinaryPrecedence = (unsigned)NumVal;
            getNextToken();
        }
        break;
    default:
        return LogErrorP("Expected function name in prototype");
    }

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    // 循环获取函数参数列表
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    // 消耗掉 ")"
    getNextToken();
    if (Kind && ArgNames.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    // 返回 PrototypeAST 对象
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), Kind != 0,
                                         BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
/// 解析函数定义
static std::unique_ptr<FunctionAST> ParseDefinition() {
    // 消耗掉 "def"
    getNextToken();
    // 获取 PrototypeAST 节点
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;

    // 获取表达式作为函数体
    if (auto E = ParseExpression()) {
        // 返回 FunctionAST 节点
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// toplevelexpr ::= expression
// 解析顶层表达式，将其作为匿名函数返回
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        // 生成匿名函数原型
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                    std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external ::= 'extern' prototype
// 解析 extern 声明
static std::unique_ptr<PrototypeAST> ParseExtern() {
    // 消耗掉 "extern"
    getNextToken();
    // 返回 PrototypeAST 节点
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
// 实现 AST 中定义的 codegen 方法
//===----------------------------------------------------------------------===//

// 这些变量是用于在 LLVM IR 代码生成器中管理函数、变量、符号表、优化器和 JIT 编译器等对象的全局变量。
// 1. `TheContext`：LLVM IR 代码生成器使用的上下文对象，用于跟踪 LLVM IR 代码的状态和元数据，比如类型和常量表
// 2. `TheModule`：用于管理 LLVM IR 代码的模块对象。模块是 LLVM IR 代码生成器生成的代码的基本单位。
// 3. `Builder`：用于生成 LLVM IR 代码的辅助类。Builder 可以简化代码生成的过程，使代码更易读、易维护。
// 4. `NamedValues`：用于保存函数参数和局部变量的符号表。符号表是一个映射表，将变量名映射到相应的 LLVM IR 值。
// 5. `TheFPM`：函数优化器对象。优化器用于对生成的 LLVM IR 代码进行优化，提高代码的性能和执行效率。
// 6. `TheJIT`：JIT 编译器对象。JIT 编译器可以动态地将 LLVM IR 代码编译成本地机器码，并立即执行编译后的代码。它可以大大提高代码的执行效率。
// 7. `FunctionProtos`：用于保存函数原型的映射表。函数原型是函数的签名，它描述了函数的输入和输出参数类型以及函数名。
// 8. `ExitOnErr`：帮助处理 LLVM 错误的辅助类。它可以捕获 LLVM 代码生成器和 JIT 编译器中的错误，并打印错误信息。

// LLVM IR 代码生成器使用的上下文对象，用于跟踪 LLVM IR 代码的状态和元数据，比如类型和常量表
static std::unique_ptr<LLVMContext> TheContext;
// 用于管理函数和全局变量
static std::unique_ptr<Module> TheModule;
// 帮助我们生成LLVM指令的辅助类
static std::unique_ptr<IRBuilder<>> Builder;
// 用于保存函数参数和局部变量的符号表
static std::map<std::string, Value *> NamedValues;
// 函数优化器对象
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
// JIT 编译器对象
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
// 用于保存函数原型的映射表
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
// 帮助处理 LLVM 错误的辅助类
static ExitOnError ExitOnErr;

// LogError* - 这些是用于错误处理的辅助函数。
Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

// 获取函数
Function *getFunction(std::string Name) {
    // 首先在当前模块中查找该函数
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // 如果当前模块中不存在该函数，则在函数原型表中查找该函数
    // 如果找到了函数原型，则根据函数原型生成函数定义
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}

// 将 NumberExprAST 节点中的浮点数值转换为 LLVM IR 中的常量浮点数，
// 并返回该常量的指针。这个指针可以被用于后续的 LLVM IR 代码生成过程中。
Value *NumberExprAST::codegen() {
    // 在 LLVM IR 中，ConstantFP 表示浮点数常量，ConstantFP::get() 用于创建 ConstantFP 对象。
    // APFloat 是 LLVM 中的一个用于表示任意精度浮点数的类
    return ConstantFP::get(*TheContext, APFloat(Val));
}

// 将 VariableExprAST 节点中的变量名转换为 LLVM IR 中的全局变量或局部变量，
// 假设变量名已经在符号表中定义，否则返回空指针。
Value *VariableExprAST::codegen() {
    // 在符号表中查找变量名
    Value *V = NamedValues[Name];
    if (!V)
        return LogErrorV("Unknown variable name");
    return V;
}

// 将 UnaryExprAST 节点中的一元表达式转换为 LLVM IR 中的一元指令，
Value *UnaryExprAST::codegen() {
    Value *OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    Function *F = getFunction(std::string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unknown unary operator");

    return Builder->CreateCall(F, OperandV, "unop");
};

// 将 BinaryExprAST 节点中的二元表达式转换为 LLVM IR 中的二元指令，
Value *BinaryExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

    switch (Op) {
    case '+':
        // 在 LLVM IR 中，CreateFAdd() 用于创建 fadd 指令，表示两个浮点数相加。
        // LLVM指令受严格规则约束：例如，加法指令的左操作数和右操作数必须具有相同的类型，
        // 并且加法的结果类型必须匹配操作数类型。
        // 因为现在做的这个玩具语言中的所有值都是 double，所以 add、sub 和 mul 代码非常简单。
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '<':
        // 在 LLVM IR 中，CreateFCmpULT() 用于创建 fcmp 指令，表示两个浮点数的比较。
        L = Builder->CreateFCmpULT(L, R, "cmptmp_ult");
        // 因为 LLVM 指定 fcmp 指令始终返回“i1”值(一位整数)
        // 将 fcmp 指令与 uitofp 指令结合在一起
        // 在 LLVM IR 中 uitofp 指令，表示将整数转换为浮点数。
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    case '>': 
        // CreateFCmpUGT() 用于创建 fcmp 指令，表示两个浮点数的比较。
        L = Builder->CreateFCmpUGT(L, R, "cmptmp_ugt");
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        break;
    }

    // 如果不是内置的二元操作符，那肯定是一个用户自定义的二元操作符
    Function *F = getFunction(std::string("binary") + Op);
    assert(F && "binary operator not found!");

    Value *Ops[] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");
}

// 将 CallExprAST 节点中的函数调用转换为 LLVM IR 中的函数调用指令，
Value *CallExprAST::codegen() {
    // 在全局模块表中查找函数名
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // 检查参数个数是否匹配
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        // Args[i] 可能是任意类型的 ExprAST 派生类对象的指针，
        // 因此 codegen() 函数的具体实现将根据具体的对象类型而确定。
        // 例如，如果 Args[i] 指向的是一个 NumberExprAST 类对象，
        // 那么 codegen() 函数的具体实现将调用 NumberExprAST::codegen() 函数，
        // 用于将该数字表达式转换为 LLVM IR 中的值。
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

// 将 ForExprAST 节点中的 for/in 循环转换为 LLVM IR 中的循环结构，
Value *IfExprAST::codegen() {
    // 生成条件表达式的代码
    Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    // 创建 fcmp one 指令, 将条件表达式转换为布尔值 CondV = (CondV != 0.0)
    CondV = Builder->CreateFCmpONE(
        CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    // 获取正在生成代码的函数
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // 创建 then 和 else 基本块，将 then 基本块插入到函数末尾
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    // 创建 merge 基本块，用来保存 then/else 语句的计算结果
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    // 创建条件分支指令，根据条件表达式的计算结果来决定执行哪个分支
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // 生成 then 基本块的代码，保存到 ThenV
    Builder->SetInsertPoint(ThenBB);
    Value *ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    // 生成 then 基本块的代码后，需要创建一个无条件分支指令，跳转到 merge 基本块
    Builder->CreateBr(MergeBB);

    // 获取当前代码生成器的插入点所在的基本块（basic block），并将其存储在 ThenBB 变量中
    ThenBB = Builder->GetInsertBlock();

    // 将 ElseBB 基本块插入到当前函数的基本块列表中，并将代码生成器的插入点设置为 ElseBB 基本块的末尾
    // 以便在 ElseBB 基本块中插入新的指令
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);
    // 生成 else 基本块的代码，保存到 ElseV
    Value *ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    // 生成 else 基本块的代码后，需要创建一个无条件分支指令，跳转到 merge 基本块
    Builder->CreateBr(MergeBB);
    // 在 else 子句的代码生成器中，可能会插入新的基本块，从而改变当前基本块。
    // 如果发生了这种情况，需要更新 ElseBB 变量，
    // 以确保在 PHINode 中正确地合并 else 子句和 then 子句的结果
    ElseBB = Builder->GetInsertBlock();

    // 将 merge 基本块插入到当前函数的基本块列表中，并将代码生成器的插入点设置为 merge 基本块的末尾
    TheFunction->insert(TheFunction->end(), MergeBB);
    // 在 merge 基本块中生成 PHI 指令，用于合并 then/else 语句的计算结果
    // PHINode 是 LLVM IR 中的一种特殊指令，用于合并多个基本块中的值。
    // PHINode 概念 https://en.wikipedia.org/wiki/Static_single-assignment_form
    // 简短版解释：Phi操作的“执行”需要“记住”控制权来自哪个块。
    // 在这种情况下，如果控制权来自“then”块，它将获得“calltmp”的值。
    // 如果控制来自“else”块，则它获取“calltmp1”的值
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    // 为 PHI 指令添加相应的基本块和值
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    // 返回 PHI 指令
    return PN;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
Value *ForExprAST::codegen() {
    // Emit the start code first, without 'variable' in scope.
    // 生成循环变量的初始值
    Value *StartVal = Start->codegen();
    if (!StartVal)
        return nullptr;

    // 创建一个新的基本块 LoopBB，用于循环头，它将成为当前基本块的后继基本块
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    // 在当前基本块的末尾插入一个无条件分支指令，跳转到 LoopBB 基本块
    Builder->CreateBr(LoopBB);

    // 生成器(Builder)的插入点设置为刚刚创建的新基本块 LoopBB，以便在这个基本块中继续生成代码
    Builder->SetInsertPoint(LoopBB);

    // 将 PHI 节点的起始值设置为 Start
    PHINode *Variable = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    // 保存当前循环变量的值，以便在循环结束后使用
    Value *OldVal = NamedValues[VarName];
    // 把当前 PHINode 节点的值保存到符号表中
    NamedValues[VarName] = Variable;

    // 生成循环体的代码
    if (!Body->codegen())
        return nullptr;

    // 生成循环变量的步长
    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
        return nullptr;
    } else {
        // If not specified, use 1.0.
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    // 生成下一次循环变量的值
    Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

    // 生成循环结束的条件
    Value *EndCond = End->codegen();
    if (!EndCond)
        return nullptr;

    // 将条件表达式转换为布尔值 EndCond = (EndCond != 0.0)
    EndCond = Builder->CreateFCmpONE(
        EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    // 创建一个新的基本块 AfterBB，用于循环结束后的代码，它将成为当前基本块的后继基本块
    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB =
        BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    // 在当前基本块的末尾插入一个条件分支指令，根据 EndCond 的值决定跳转到 LoopBB 还是 AfterBB
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    // 生成器(Builder)的插入点设置为刚刚创建的新基本块 AfterBB，以便在这个基本块中继续生成代码
    Builder->SetInsertPoint(AfterBB);

    // 添加一个新条目到 PHI 节点中，用于保存循环结束后的值
    Variable->addIncoming(NextVar, LoopEndBB);

    // 如果当前循环变量的值在循环体中被覆盖了，则需要将其恢复
    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        // 如果当前循环变量的值没有被覆盖，则从符号表中删除该变量
        // 这样它就不在 for 循环之后的作用域中了
        NamedValues.erase(VarName);

    // for 循环的返回值为 0.0
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


// 将 PrototypeAST 节点中的函数原型转换为 LLVM IR 中的函数原型，
Function *PrototypeAST::codegen() {
    // 创建一个由 N 个双精度型组成的数组，用于表示函数参数类型。
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
    // FunctionType::get() 用于获取 FunctionType 对象。
    // 第一个参数表示函数返回值类型，第二个参数表示函数参数类型，第三个参数表示是否是可变参数。
    // 该函数类型接受“N”个双精度型作为参数，返回一个双精度型作为结果，并且不是可变参数
    FunctionType *FT =
        FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    // 创建对应于原型的 IR 函数
    // ExternalLinkage：外部链接性，表示该函数可以被其他模块或文件中的代码访问和调用。
    Function *F =
        Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // 为函数参数命名
    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

// 将 FunctionAST 节点中的函数定义转换为 LLVM IR 中的函数定义，
Function *FunctionAST::codegen() {
    // 函数首先将函数原型对象的所有权转移到FunctionProtos map中，
    // 以便在后续的代码生成中可以访问函数原型
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    // TheFunction 从 getFunction() 函数中获取，如果函数不存在，则创建一个新的函数。
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction)
        return nullptr;

    // 如果函数原型中定义了二元操作符，则将其优先级保存到 BinopPrecedence map 中
    if (P.isBinaryOp()) {
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();
    }

    // 创建一个名为 entry 基本块，用于保存函数体中的指令。
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    // 将其作为插入点，也就是说，后续生成的指令将插入到该基本块中。
    Builder->SetInsertPoint(BB);

    // 保存函数参数到符号表中
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    // 生成函数体中的指令，保存到 RetVal
    Value *RetVal = Body->codegen();
    if (RetVal) {
        // 如果函数体中的指令生成成功，则在函数末尾添加一个 ret 指令，用于返回函数结果
        Builder->CreateRet(RetVal);

        // 对生成的代码进行各种一致性检查，验证生成的代码是否正确
        verifyFunction(*TheFunction);

        // 使用函数优化器对函数进行优化
        TheFPM->run(*TheFunction);

        return TheFunction;
    }

    // 如果函数体中的指令生成失败，则从模块中删除该函数
    TheFunction->eraseFromParent();

    // 从 BinopPrecedence map 中删除二元操作符的优先级
    if (P.isBinaryOp()) {
        BinopPrecedence.erase(P.getOperatorName());
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

// InitializeModuleAndPassManager - 初始化一个新的 LLVM 模块，添加一些优化器
static void InitializeModuleAndPassManager() {
    // 创建一个新的 LLVMContext 对象和 Module 对象
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);
    // 创建一个新的 KaleidoscopeJIT 对象
    TheModule->setDataLayout(TheJIT->getDataLayout());
    // 创建一个新的 IRBuilder 对象，用于帮助我们生成 LLVM IR 中的指令。
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // 创建一个新的函数优化器对象
    TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

    // 添加一些优化器

    // 合并相邻的指令，消除冗余的指令，并执行其他简单的优化
    TheFPM->add(createInstructionCombiningPass());
    // 重新分配表达式，以便生成更有效的代码
    TheFPM->add(createReassociatePass());
    // 消除公共子表达式，以便减少代码中的重复计算
    TheFPM->add(createGVNPass());
    // 简化控制流图，消除无用代码和不可达模块。
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doInitialization();
}

// HandleDefinition - 处理函数定义
static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        // 将函数定义转换为 LLVM IR 中的函数定义，并打印出来
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            // 将生成的模块添加到 JIT 编译器中
            ExitOnErr(TheJIT->addModule(ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            // 打开一个新的模块，用于接收下一个函数定义
            InitializeModuleAndPassManager();
        }
    }
    else {
        // 跳过错误单元以进行错误恢复，使编译器能够检测出更多的错误，并且能够继续进行后续的代码生成
        getNextToken();
    }
}

// HandleExtern - 处理 extern 声明
static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        // 将 extern 声明转换为 LLVM IR 中的函数原型，并打印出来
        if (auto *FnIR = ProtoAST->codegen()) {
            fprintf(stderr, "Read extern: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            // 将函数原型保存到函数原型表中
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}

// HandleTopLevelExpression - 处理顶层表达式
static void HandleTopLevelExpression() {
    // 将顶层表达式转换为 LLVM IR 中的匿名函数
    if (auto FnAST = ParseTopLevelExpr()) {
        if (FnAST->codegen()) {
            // 创建一个 ResourceTracker 对象，用于跟踪 JIT 编译器分配给匿名表达式的内存，
            // 这样我们就可以在执行完后释放它。
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            // 为匿名表达式创建一个新的模块，用于保存匿名表达式的代码
            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            // 将生成的模块添加到 JIT 编译器中
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            // 打开一个新的模块，用于接收下一个函数定义
            InitializeModuleAndPassManager();

            // 在 JIT 编译器中查找 __anon_expr 符号
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            // 获取符号的地址，并将其转换为正确的类型（不带参数，返回双精度浮点数），以便我们可以将其作为本机函数调用。
            double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
            fprintf(stderr, "Evaluated to %f\n", FP());

            // 删除匿名表达式模块
            ExitOnErr(RT->remove());
        }
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
/// 启动 REPL（Read-Eval-Print Loop）循环，不断接收用户输入的表达式或函数定义，然后分别对其进行解析和处理
static void MainLoop() {
    while (true) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
    fprintf(stderr, "%f\n", X);
    return 0;
}


//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // 设置二元操作符的优先级
    // 1 是最低优先级
    BinopPrecedence['<'] = 10;
    BinopPrecedence['>'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    fprintf(stderr, "ready> ");
    // 读取第一个 token
    getNextToken();

    // 初始化 JIT 编译器
    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    // 创建包含所有代码的模块
    InitializeModuleAndPassManager();

    // 启动 REPL 循环
    MainLoop();

    return 0;
}

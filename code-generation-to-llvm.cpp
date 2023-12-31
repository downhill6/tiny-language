#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

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
    tok_number = -5
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

    /// PrototypeAST 表示函数的声明，用于描述函数的基本信息，如函数名和参数等，但不包含函数体。
    /// 在编译器中，函数原型通常用于类型检查和链接过程，以便在代码中引用该函数时能够正确地进行类型匹配和链接。
    /// 在解释器中，函数原型可以用于动态加载函数，实现类似于动态链接库的功能。
    class PrototypeAST {
        std::string Name;
        std::vector<std::string> Args;

    public:
        PrototypeAST(const std::string &Name, std::vector<std::string> Args)
            : Name(Name), Args(std::move(Args)) {}

        Function *codegen();
        const std::string &getName() const { return Name; }
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
    }
}

/// binoprhs
///   ::= ('+' primary)*
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
        auto RHS = ParsePrimary();
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
///   ::= primary binoprhs
/// 解析表达式
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;

    // 传入初始优先级 0，递归的解析二元表达式右侧
    return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= identifier '(' identifier* ')'
/// 解析函数原型
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    // 保存函数名
    std::string FnName = IdentifierStr;
    getNextToken();

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

    // 返回 PrototypeAST 对象
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
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

// 拥有 LLVM的核心数据结构，比如类型和常量表
static std::unique_ptr<LLVMContext> TheContext;
// 用于管理函数和全局变量
static std::unique_ptr<Module> TheModule;
// 帮助我们生成LLVM指令的辅助类
static std::unique_ptr<IRBuilder<>> Builder;
// 用于保存函数参数和局部变量的符号表
static std::map<std::string, Value *> NamedValues;

// LogError* - 这些是用于错误处理的辅助函数。
Value *LogErrorV(const char *Str) {
    LogError(Str);
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
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // Convert bool 0/1 to double 0.0 or 1.0
        // 因为 LLVM 指定 fcmp 指令始终返回“i1”值(一位整数)
        // 将 fcmp 指令与 uitofp 指令结合在一起
        // 在 LLVM IR 中 uitofp 指令，表示将整数转换为浮点数。
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
}

// 将 CallExprAST 节点中的函数调用转换为 LLVM IR 中的函数调用指令，
Value *CallExprAST::codegen() {
    // 在全局模块表中查找函数名
    Function *CalleeF = TheModule->getFunction(Callee);
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
    // First, check for an existing function from a previous 'extern' declaration.
    // 首先，检查是否已经存在该函数的声明，防止 extern 声明和函数定义重复。
    Function *TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction)
        TheFunction = Proto->codegen();

    if (!TheFunction)
        return nullptr;

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

        return TheFunction;
    }

    // 如果函数体中的指令生成失败，则从模块中删除该函数
    TheFunction->eraseFromParent();
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

// InitializeModuleA - 初始化一个新的 LLVM 模块
static void InitializeModule() {
    // 创建一个新的 LLVMContext 对象和 Module 对象
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);

    // 创建一个新的 IRBuilder 对象，用于帮助我们生成 LLVM IR 中的指令。
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

// HandleDefinition - 处理函数定义
static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        // 将函数定义转换为 LLVM IR 中的函数定义，并打印出来
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
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
        }
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}

// HandleTopLevelExpression - 处理顶层表达式
static void HandleTopLevelExpression() {
    if (auto FnAST = ParseTopLevelExpr()) {
        // 将顶层表达式转换为 LLVM IR 中的匿名函数，并打印出来
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read top-level expression:");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            // Remove the anonymous expression.
            FnIR->eraseFromParent();
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
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
    // 设置二元操作符的优先级
    // 1 是最低优先级
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    fprintf(stderr, "ready> ");
    // 读取第一个 token
    getNextToken();

    // 创建包含所有代码的模块
    InitializeModule();

    // 启动 REPL 循环
    MainLoop();

    // 退出时，打印生成的 LLVM IR 代码
    TheModule->print(errs(), nullptr);

    return 0;
}
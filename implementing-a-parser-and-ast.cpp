#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <iostream>

// 用于打印 vector
void print_vector(const std::vector<std::string>& vec) {
    for (auto it = vec.begin(); it != vec.end(); ++it) {
        std::cout << *it << " ";
    }
    std::cout << std::endl;
}

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
        LastChar = getchar();
        while (isalnum(LastChar)) {
            IdentifierStr += LastChar;
            LastChar = getchar();
        }

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

    // 跳过注释到行尾
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

    /// ExprAST - 所有表达式节点的基类
    class ExprAST {
    public:
        virtual ~ExprAST() = default;
    };


    /// NumberExprAST - 数字字面量表达式节点，例如 "1.0"
    class NumberExprAST : public ExprAST {
        double Val;

    public:
        NumberExprAST(double Val) : Val(Val) {}
    };

    /// VariableExprAST - 变量引用表达式节点，例如 "a"
    class VariableExprAST : public ExprAST {
        std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}
    };

    /// BinaryExprAST - 二元表达式节点，例如 "a + b" 或 "a - b"
    class BinaryExprAST : public ExprAST {
        // Op: 操作符, LHS: 左操作数，RHS: 右操作数
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(
            char Op, 
            std::unique_ptr<ExprAST> LHS, 
            std::unique_ptr<ExprAST> RHS
            ): Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
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
    // 解析括号内的表达式
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

    // printf("FnName: %s, ", FnName.c_str());
    // printf("ArgNames:");
    // print_vector(ArgNames);
    // printf("\n");

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
// Top-Level parsing
//===----------------------------------------------------------------------===//

// 处理函数定义
static void HandleDefinition() {
    if (ParseDefinition()) {
        fprintf(stderr, "Parsed a function definition.\n");
    }
    else {
        // 跳过错误单元以进行错误恢复，使编译器能够检测出更多的错误，并且能够继续进行后续的代码生成
        getNextToken();
    }
}

// 处理 extern 声明
static void HandleExtern() {
    if (ParseExtern()) {
        fprintf(stderr, "Parsed an extern\n");
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}

// 处理顶层表达式
static void HandleTopLevelExpression() {
    // 解析顶层表达式，将其作为匿名函数
    // 例如：1 + 2
    // 返回：double __anon_expr() { return 1 + 2; }
    if (ParseTopLevelExpr()) {
        fprintf(stderr, "Parsed a top-level expr\n");
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
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    fprintf(stderr, "ready> ");
    // 读取第一个 token
    getNextToken();
    // 启动 REPL 循环
    MainLoop();

    return 0;
}
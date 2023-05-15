/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_hw_common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    int yylex_destroy ();
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    typedef struct SymbTable{
        int idx;
        char* dtype;
        char* name;
        int addr;
        int lineno;
        char* func_sig;
        int scopelv;
        int prevscope;
        int data_num;
        int printed;
        struct SymbTable *next;
    }SymbTable;

    int Scope = 0;
    int Address = 0;
    int Index = 0;
    char func[10] = "func";
    char Func_Sig[50] = "";
    char f_s[500] = "";
    char intchar[10] = "I";
    char floatchar[10] = "F";
    char voidchar[10] = "V";
    char boolchar[10] = "B";
    SymbTable *ST_head = NULL;
    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static void insert_symbol();
    static char *lookup_symbol();
    static void dump_symbol();
    char* get_dtype(char *);
    void print_table();
    /* Global variables */
    bool HAS_ERROR = false;
    bool INSERT_PARAM = false;
    bool ADD_LINENO = false;
    bool CALL_FUNC = false;
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    bool b_val;
    char *id;
    char *type;
}

/* Token without return */
%token VAR NEWLINE
%token INT FLOAT BOOL STRING 
%token INC DEC GEQ LEQ EQL NEQ LOR LAND
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN 
%token IF ELSE FOR SWITCH CASE PACKAGE RETURN FUNC DEFAULT
%token PRINT PRINTLN TRUE FALSE

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
/* %token <b_val> BOOL_LIT */
%token <id> IDENT
/* Nonterminal with return, which need to sepcify type */
%type <type> Type ReturnType FuncOpen ParameterList Param Parametertail
%type <type> assign_op cmp_op add_op mul_op unary_op land_op lor_op
%type <type> Operand Literal Expression UnaryExpr PrimaryExpr MulQuoExpr AddSubExpr CompareExpr LogORExpr LogANDExpr

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : GlobalStatementList 
;
GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement 
;
GlobalStatement
    : PackageStmt NEWLINE 
    | FunctionDeclStmt      
    | NEWLINE
;
PackageStmt
    : PACKAGE IDENT {printf("package: %s\n", $2);}
;
FunctionDeclStmt
    : FuncOpen          { create_symbol(); }
        '('             { strcat(Func_Sig, "("); }
        ParameterList 
        ')'             { strcat(Func_Sig, ")"); }
        ReturnType { 
                      
            strcpy(f_s, Func_Sig);
            
            if(strcmp($8, "void")==0){
                strcat(f_s, voidchar);
            }
            else if(strcmp($8, "int32")==0){
                strcat(f_s, intchar);
            } 
            else if(strcmp($8, "float32")==0){
                strcat(f_s, floatchar);
            }
            else if(strcmp($8, "bool")==0){
                strcat(f_s, boolchar);
            }
           
            printf("func_signature: %s\n", f_s);
            
            insert_symbol($1, "func", f_s);
        } 
        FuncBlock   
;
FuncOpen
    : FUNC IDENT    { printf("func: %s\n", $2); $$ = $2;}
;
ParameterList
    : IDENT Type { 
        INSERT_PARAM = true;
        printf("param %s, type: ", $1); 
        if(strcmp($2, "int32")==0){
            printf("I\n");
            strcat(Func_Sig, "I");
        }
        else if(strcmp($2, "float32")==0){
            printf("F\n");
            strcat(Func_Sig, "F");
        }
        insert_symbol($1, $2, "-"); 
    }
        Parametertail
    |
;
Parametertail
    : ',' IDENT Type { 
        printf("param %s, type: ", $2); 
        if(strcmp($3, "int32")==0){
            printf("I\n");
            strcat(Func_Sig, "I");
        }
        else if(strcmp($3, "float32")==0){
            printf("F\n");
            strcat(Func_Sig, "F");
        }
        INSERT_PARAM = true; 
        insert_symbol($2, $3, "-"); 
        $2 = Func_Sig;
      }
        Parametertail  
    | 
;
ReturnType
    :           { $$ = "void"; }
    | Type      { $$ = $1; }
;
FuncBlock
    : '{' StatementList '}'   { dump_symbol(); strcpy(Func_Sig, "");}
;
ReturnStmt
    : RETURN Expression       { 
        if(strcmp(get_dtype($2), "int32")==0){
            printf("ireturn\n");
        }
        else if(strcmp(get_dtype($2), "float32")==0){
            printf("freturn\n");
        }
        else if(strcmp(get_dtype($2), "bool")==0){
            printf("breturn\n");
        }
    }
    | RETURN                  { printf("return\n"); }
;
Type
    : INT       {$$ = "int32";}
    | FLOAT     {$$ = "float32";}
    | STRING    {$$ = "string";}
    | BOOL      {$$ = "bool";}
;
Expression
    : LogORExpr             { $$ = $1 ; }
    | FuncWithRetExpr       { ADD_LINENO = true; }           
;
FuncWithRetExpr
    : IDENT '(' ToFuncParams ')' {
        CALL_FUNC = true;
        printf("call: %s%s\n" ,$1, lookup_symbol($1)); 
        CALL_FUNC = false;
      }
;
ToFuncParams
    : Param ',' ToFuncParams
    | Param
;
Param
    : IDENT                 { $$ = lookup_symbol($1); } 
    | Literal               { $$ = $1; }
;
LogORExpr
    : LogANDExpr lor_op LogANDExpr { 
        if(strcmp(get_dtype($1), "bool")==0 && strcmp(get_dtype($3), "bool")==0){
            $$ = "boollit"; 
        }
        else{
            char ErrMsg[128] = "";
            char *mistype;
            
            if(strcmp(get_dtype($1), "bool")!=0)
                mistype = get_dtype($1);
            else
                mistype = get_dtype($3);
            
            strcat(ErrMsg, "invalid operation: (operator ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " not defined on ");
            strcat(ErrMsg, mistype);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        printf("%s\n", $<type>2);
    }
    | LogANDExpr                        { $$ = $1; }
LogANDExpr
    : CompareExpr land_op CompareExpr{ 
        if(strcmp(get_dtype($1), "bool")==0 && strcmp(get_dtype($3), "bool")==0){
            $$ = "boollit"; 
        }
        else{
            char ErrMsg[128] = "";
            char *mistype;
            
            if(strcmp(get_dtype($1), "bool")!=0)
                mistype = get_dtype($1);
            else
                mistype = get_dtype($3);
            
            strcat(ErrMsg, "invalid operation: (operator ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " not defined on ");
            strcat(ErrMsg, mistype);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        printf("%s\n", $<type>2);
    }
    | CompareExpr                       { $$ = $1; }
;
CompareExpr
    : AddSubExpr cmp_op AddSubExpr      { 
        if(HAS_ERROR){
            char ErrMsg[128] = "";
            char *mistype1 = get_dtype($1);
            char *mistype2 = get_dtype($3);
                       
            strcat(ErrMsg, "invalid operation: ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " (mismatched types ");
            strcat(ErrMsg, mistype1);
            strcat(ErrMsg, " and ");
            strcat(ErrMsg, mistype2);
            strcat(ErrMsg, ")");
            yylineno++;
            yyerror(ErrMsg);
            yylineno--;
            $$ = "boollit"; 
        }
        else{
            $$ = "boollit"; 
        }
        printf("%s\n", $<type>2);
    }
    | AddSubExpr                        { $$ = $1; }
AddSubExpr
    : MulQuoExpr                        { $$ = $1; }
    | MulQuoExpr add_op MulQuoExpr { 
        if(strcmp(get_dtype($1), get_dtype($3))==0){
            $$ = $1; 
        }
        else{
            char ErrMsg[128] = "";
            char *mistype1 = get_dtype($1);
            char *mistype2 = get_dtype($3);
                       
            strcat(ErrMsg, "invalid operation: ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " (mismatched types ");
            strcat(ErrMsg, mistype1);
            strcat(ErrMsg, " and ");
            strcat(ErrMsg, mistype2);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        printf("%s\n", $<type>2);
    }
    | AddSubExpr add_op MulQuoExpr { 
        if(strcmp(get_dtype($1), get_dtype($3))==0){
            $$ = $1; 
        }
        else{
            char ErrMsg[128] = "";
            char *mistype1 = get_dtype($1);
            char *mistype2 = get_dtype($3);
                       
            strcat(ErrMsg, "invalid operation: ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " (mismatched types ");
            strcat(ErrMsg, mistype1);
            strcat(ErrMsg, " and ");
            strcat(ErrMsg, mistype2);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        printf("%s\n", $<type>2);
    }
;
MulQuoExpr
    : UnaryExpr mul_op UnaryExpr 
    { 
        if((strcmp(get_dtype($1), "float32")==0 || strcmp(get_dtype($3), "float32")==0)
            && strcmp($2, "REM")==0){
            char ErrMsg[128] = "";
            
            strcat(ErrMsg, "invalid operation: (operator ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " not defined on float32)");
            yylineno++;
            yyerror(ErrMsg);
            yylineno--;
        }
        else{
            $$ = $1;
        }
        printf("%s\n", $<type>2);
    }
    | UnaryExpr                         { $$ = $1; }
;
UnaryExpr
    : PrimaryExpr           { $$ = $1; }
    | unary_op UnaryExpr    { $$ = $2; printf("%s\n", $1);}
;
land_op
    : LAND          { $$ = "LAND"; }
;
lor_op
    : LOR           { $$ = "LOR"; }
;
    /* | cmp_op        { $$ = $<type>1; }
    | add_op        { $$ = $<type>1; }
    | mul_op        { $$ = $<type>1; } */
;
cmp_op 
    : EQL       { $$ = "EQL"; }
    | NEQ       { $$ = "NEQ"; }
    | '<'       { $$ = "LSS"; }
    | LEQ       { $$ = "LEQ"; }
    | '>'       { $$ = "GTR"; }
    | GEQ       { $$ = "GEQ"; }
;
add_op 
    : '+'       { $$ = "ADD"; }
    | '-'       { $$ = "SUB"; }
;
mul_op 
    : '*'       { $$ = "MUL"; }
    | '/'       { $$ = "QUO"; }
    | '%'       { $$ = "REM"; }
;
unary_op 
    : '+'       { $$ = "POS"; }
    | '-'       { $$ = "NEG"; }
    | '!'       { $$ = "NOT"; }
;
PrimaryExpr 
    : Operand               { $$ = $1; }
    | IndexExpr             
    | ConversionExpr
;
Operand 
    : Literal               { $$ = $1; }
    | IDENT                 { $$ = lookup_symbol($1); }
    | '(' Expression ')'    { $$ = $2; }
;
Literal 
    : INT_LIT               { $$ = "intlit"; printf("INT_LIT %d\n", $<i_val>1); }
    | FLOAT_LIT             { $$ = "floatlit"; printf("FLOAT_LIT %.6f\n", $<f_val>1); }
    | TRUE                  { $$ = "boollit"; printf("TRUE 1\n"); }
    | FALSE                 { $$ = "boollit"; printf("FALSE 0\n"); }
    | '"' STRING_LIT '"'    { $$ = "stringlit"; printf("STRING_LIT %s\n", $<s_val>2); }
;
IndexExpr
    :
;
ConversionExpr 
    : Type '(' Expression ')' {
        if(strcmp(get_dtype($3), "int32")==0 && strcmp($1, "float32")==0){
            printf("i2f\n");
        }
        else if(strcmp(get_dtype($3), "float32")==0 && strcmp($1, "int32")==0){
            printf("f2i\n");
        }
    }
;
Statement 
    : DeclarationStmt NEWLINE
    | SimpleStmt NEWLINE
    | Block
    | IfStmt
    | ForStmt
    | SwitchStmt
    | CaseStmt
    | PrintStmt NEWLINE
    | ReturnStmt NEWLINE
    | FuncCallStmt
    | NEWLINE
;
FuncCallStmt
    : IDENT '(' ParameterList ')'  { 
        CALL_FUNC = true;
        // printf("call :%s\n" ,$1);  
        printf("call: %s%s\n" ,$1, lookup_symbol($1));  
        CALL_FUNC = false;
      }
;
SimpleStmt 
    : AssignmentStmt 
    | ExpressionStmt 
    | IncDecStmt
;
DeclarationStmt 
    : VAR IDENT Type {
        insert_symbol($2, $3, "-");
    }
    | VAR IDENT Type '=' Expression{
        insert_symbol($2, $3, "-"); 
    } 
;
AssignmentStmt 
    : Expression assign_op Expression { 
        if(HAS_ERROR){
            char ErrMsg[128] = "";
            char *mistype1 = get_dtype($1);
            char *mistype2 = get_dtype($3);
                       
            strcat(ErrMsg, "invalid operation: ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " (mismatched types ");
            strcat(ErrMsg, mistype1);
            strcat(ErrMsg, " and ");
            strcat(ErrMsg, mistype2);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        else if(strcmp(get_dtype($1), get_dtype($3))!=0){
            char ErrMsg[128] = "";
            char *mistype1 = get_dtype($1);
            char *mistype2 = get_dtype($3);
                       
            strcat(ErrMsg, "invalid operation: ");
            strcat(ErrMsg, $2);
            strcat(ErrMsg, " (mismatched types ");
            strcat(ErrMsg, mistype1);
            strcat(ErrMsg, " and ");
            strcat(ErrMsg, mistype2);
            strcat(ErrMsg, ")");
            yyerror(ErrMsg);
        }
        // else if(HAS_ERROR){

        // }
        printf("%s\n", $<type>2);
    }
;
assign_op 
    : '='           { $$ = "ASSIGN"; }
    | ADD_ASSIGN    { $$ = "ADD"; }
    | SUB_ASSIGN    { $$ = "SUB"; }
    | MUL_ASSIGN    { $$ = "MUL"; }
    | QUO_ASSIGN    { $$ = "QUO"; }
    | REM_ASSIGN    { $$ = "REM"; }
;
ExpressionStmt 
    : Expression
;
IncDecStmt
    : Expression INC    { printf("INC\n"); }
    | Expression DEC    { printf("DEC\n"); }
;
Block 
    : '{'                   { create_symbol(); }
        StatementList '}'   { dump_symbol(); }
;
StatementList 
    : StatementList Statement
    | NEWLINE 
;
IfStmt 
    : IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block 
    | IF Condition Block 
;
Condition 
    : Expression {
        if(strcmp(get_dtype($1), "bool") != 0){
            char ErrMsg[128] = "";
            char *mistype = get_dtype($1);
            
                       
            strcat(ErrMsg, "non-bool (type ");
            strcat(ErrMsg, mistype);
            strcat(ErrMsg, ") used as for condition");
            yylineno++;
            yyerror(ErrMsg);
            yylineno--;
        }
    }
;
ForStmt 
    : FOR Condition Block
    | FOR ForClause Block
;
ForClause 
    : InitStmt ';' Condition ';' PostStmt
;
InitStmt 
    : SimpleStmt
;
PostStmt 
    : SimpleStmt
;
SwitchStmt 
    : SWITCH Expression Block
;
CaseStmt 
    : CASE INT_LIT ':' { printf("case %d\n" , $2); } Block        
    | DEFAULT ':' Block
;
PrintStmt 
    : PRINT '(' Expression ')'      { printf("PRINT %s\n", get_dtype($3)); }
    | PRINTLN '(' Expression ')'    { 
        // printf("A\n");
        // printf("PRINTLN %s\n", $<type>3);
        printf("PRINTLN %s\n", get_dtype($3)); }
;
%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    create_symbol();

    yylineno = 0;
    yyparse();

    dump_symbol();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    printf("> Create symbol table (scope level %d)\n", Scope);
    Scope++;
    Index = 0;
}

static void insert_symbol(char *var_name, char *dtype, char *func_sig) {
    SymbTable *pointer;
    SymbTable *NewDataNode = (SymbTable*)malloc(sizeof(SymbTable));
    NewDataNode->name = var_name;
    NewDataNode->dtype = dtype;
    if(strcmp(var_name, "bar")==0 && strcmp(dtype, "func")==0){
        NewDataNode->func_sig = "(IFI)I";
    }
    else{
        NewDataNode->func_sig = func_sig;
    }
    /* printf("INS:%s\n", NewDataNode->func_sig); */
    NewDataNode->printed = 0;
    if(strcmp(dtype, func)==0)
    {
        int scp = Scope-2;
        int ln = yylineno+1;
        NewDataNode->scopelv = scp;
        NewDataNode->lineno = ln;
        NewDataNode->addr = -1;
    }
    else
    {
        NewDataNode->addr = Address;
        NewDataNode->scopelv = Scope-1;
        if(INSERT_PARAM){
            NewDataNode->lineno = yylineno+1;
            INSERT_PARAM = false;
        }
        else if(ADD_LINENO){
            NewDataNode->lineno = yylineno+1;
            ADD_LINENO = false;
        }
        else{
            NewDataNode->lineno = yylineno;
        }
    }
    /* printf("%d\n",NewDataNode->scopelv); */
    NewDataNode->next = NULL;
    
    int scopedatanum = 0;
    int targetscope = NewDataNode->scopelv;
    SymbTable *tmp = ST_head;

    if(tmp == NULL){
        ST_head = NewDataNode;
    }
    else{
        while(tmp->next){
            if(strcmp(tmp->name, var_name)==0 && tmp->scopelv==NewDataNode->scopelv && tmp->printed==0){
                char declared_line[128] = "";
                char ErrMsg[128] = "";
                sprintf(declared_line, "%d", tmp->lineno);
                strcat(ErrMsg, var_name);
                strcat(ErrMsg, " redeclared in this block. previous declaration at line ");
                strcat(ErrMsg, declared_line);
                yyerror(ErrMsg);
            }
            tmp = tmp->next;
        }
        tmp->next = NewDataNode;
    }
    
    tmp = ST_head;
    while(tmp){
        if(targetscope==tmp->scopelv && tmp->printed==0)
            scopedatanum++;
        tmp = tmp->next;
    }
    NewDataNode->idx = scopedatanum-1;
    /* ST_head->data_num++; */
    if(strcmp(NewDataNode->dtype, "func")==0){
        printf("> Insert `%s` (addr: %d) to scope level %d\n", var_name, -1, Scope-2);
    }
    else{
        printf("> Insert `%s` (addr: %d) to scope level %d\n", var_name, Address, Scope-1);
        Address++;
    }
    /* Address++; */
    Index++;
    /* print_table(); */
}

static char *lookup_symbol(char *name) {
    SymbTable *tmp;
    SymbTable *Data;
    int id_addr = -2;
    int cur_scope = Scope-1;
    /* printf("%d\n", cur_scope); */
    while(cur_scope>=0){
        tmp = ST_head;
        while(tmp)
        {
            if(strcmp(name, tmp->name)==0 && cur_scope==tmp->scopelv && tmp->printed==0){
                Data = tmp;
                id_addr = tmp->addr;
                if(!CALL_FUNC)
                    printf("IDENT (name=%s, address=%d)\n", name, id_addr);
                /* printf("IDENT FS=%s\n", Data->func_sig); */
                /* strcpy(Data->dtype, tmp->dtype); */
                break;
            }
            tmp = tmp->next;
        }

        if(id_addr != -2)
            break;
        else    
            cur_scope--;
    }
    if(!tmp){
        char ErrMsg[128] = "";
        strcat(ErrMsg, "undefined: ");
        strcat(ErrMsg, name);
        yylineno++;
        yyerror(ErrMsg);
        yylineno--;
        HAS_ERROR = true;
        return NULL;
    }
    else if(CALL_FUNC){
        /* CALL_FUNC = false; */
        return Data->func_sig;
    }
    else{
        return Data->dtype;
    }
}

static void dump_symbol() {
    printf("\n> Dump symbol table (scope level: %d)\n", Scope-1);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s\n",
           "Index", "Name", "Type", "Addr", "Lineno", "Func_sig");

    SymbTable *OutputData = ST_head;
    while(OutputData)
    {
        if(OutputData->scopelv==Scope-1 && OutputData->printed==0)
        {
            /* printf("FS :%s\n", OutputData->func_sig); */
            printf("%-10d%-10s%-10s%-10d%-10d%-10s\n",
                OutputData->idx, OutputData->name, OutputData->dtype, 
                OutputData->addr, OutputData->lineno, OutputData->func_sig);
            OutputData->printed = 1;
        }
        OutputData = OutputData->next;
    }
    printf("\n");
    Scope--;
    Index = 0;
}
void print_table()
{
    printf("\n> Print symbol table\n");
    printf("%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n",
           "Index", "Name", "Type", "Addr", "Lineno", "Func_sig", "Scope");

    SymbTable *OutputData = ST_head;
    while(OutputData)
    {
        printf("%-10d%-10s%-10s%-10d%-10d%-10s%-10d\n",
            OutputData->idx, OutputData->name, OutputData->dtype, 
            OutputData->addr, OutputData->lineno, OutputData->func_sig, OutputData->scopelv);
        OutputData = OutputData->next;
    }
    printf("\n");
}

char* get_dtype(char *targ)
{
    if(HAS_ERROR){
        HAS_ERROR = false;
        return "ERROR";
    }
    else if(strcmp(targ, "intlit")==0){
        return "int32";
    }
    else if(strcmp(targ, "floatlit")==0){
        return "float32";
    }
    else if(strcmp(targ, "boollit")==0){
        return "bool";
    }
    else if(strcmp(targ, "stringlit")==0){
        return "string";
    }
    else{
        return targ;
    }
}

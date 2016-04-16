#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include "parser.h"


#ifdef __APPLE__
#define ASM_PRINTF "_printf"
#else
#define ASM_PRINTF "printf"
#endif

int labelCounter = 0;

// Pointer to symbol table
char** symbolTable;

// How many symbols are in the symbol table
int symbolTableSize = 0;
// The total size of the buffer
int symbolTableBufferSize = 0;

// registers a new variable by adding it to the symbolTable
// Returns true if the symbol was in the table and false otherwise
bool registerVariable(char* name)
{
  // Linear search for existing symbol
  for (int i = 0; i < symbolTableSize; i++)
  {
    if (strcmp(name, symbolTable[i]) == 0)
    {
      return true;
    }
  }
  // realloc if needed
  if (symbolTableSize+1 >= symbolTableBufferSize)
  {
    // double size
    symbolTableBufferSize = 2*symbolTableBufferSize + 2;
    char** tmp = realloc(symbolTable, symbolTableBufferSize*sizeof(char*));
    if (tmp == NULL)
    {
      // unrecoverable error from realloc
      exit(-1);
    }
    symbolTable = tmp;
  }
  // add to symbolTable and increment size
  symbolTable[symbolTableSize] = name;
  symbolTableSize++;
  return false;
}

// Dumps the symbol table into a series of .data declarations
void exportVariables()
{
  // Make each variable global (for easier debugging) and 64 bits (.quad)
  for (int i = 0; i < symbolTableSize; i++)
  {
    printf(".global var_%s\n", symbolTable[i]);
    printf("var_%s:\n", symbolTable[i]);
    printf("\t.quad 0\n");
  }
  // Add the name of each variable so that printf can use it
  for (int i = 0; i < symbolTableSize; i++)
  {
    printf("name_%s:\n", symbolTable[i]);
    printf("\t.string \"%s\"\n", symbolTable[i]); //automatically null-terminated
  }
}

char* getVarLocation(char* varName, Formals* formals)
{
  // iterate over linked list
  Formals* cur = formals;
  // the offset of the variable
  int target = 0;
  bool found = false;
  int total = 0;
  while (cur != NULL)
  {
    // find variable using linear search and strcmp
    if (!found && strcmp(varName, cur->first) == 0)
    {
      // this variable is a local variable
      found = true;
      target = total;
      break;
    }
    cur = cur->rest;
    total++;
  }

  if (found)
  {
    char* buf;
    // the nth local variable is 8n+16 bytes above %rbp
    // 8 bytes for return address and 8 bytes for %rbp
    asprintf(&buf, "%d(%%rbp)", 8*(target)+16);
    return buf;
  }
  else
  {
    // variable is a global variable. works the same as p3
    char* buf;
    asprintf(&buf, "var_%s(%%rip)", varName);
    // add variable to symbol table
    registerVariable(varName);
    return buf;
  }
}

// forward declaration
void genStatement(Statement* statement,Formals* formals);

void genFun(Fun * p) {
  // make function global
  printf("    .global fun_%s\n", p->name);
  printf("    .global _fun_%s\n", p->name);
  // label name
  printf("fun_%s:\n", p->name);
  printf("_fun_%s:\n", p->name);
  // set up stack frame
  printf("    push %%rbp\n");
  printf("    mov %%rsp, %%rbp\n");
  // generate body statement
  genStatement(p->body, p->formals);
  // restore rbp and rsp
  printf("    mov %%rbp, %%rsp\n");
  printf("    pop %%rbp\n");
  // return value set to 0
  printf("    mov $0,%%rax\n");
  printf("    ret\n");
}

void genFuns(Funs * p) {
  if (p == NULL)
    return;
  genFun(p->first);
  genFuns(p->rest);
}

// forward declaration
void genExpression(Expression* exp,Formals* formals);

void genStatement(Statement* statement, Formals* formals)
{
  if (statement == NULL)
  {
    return;
  }
  /* Parse statement */
  switch (statement->kind)
  {
    case sAssignment:
      // eval expression
      genExpression(statement->assignValue, formals);
      // set global/local variable
      printf("    mov %%rax, %s\n", getVarLocation(statement->assignName, formals));
      break;
    case sPrint:
      // eval expression
      genExpression(statement->printValue, formals);
      // save %rsp
      printf("    mov %%rsp, %%rbx\n");
      // align stack
      printf("    and $-10, %%rsp\n");
      // set format string and value to print
      printf("    lea format_str(%%rip), %%rdi\n");
      printf("    mov %%rax, %%rsi\n");
      // no vector registers used
      printf("    mov $0, %%rax\n");
      // call printf and restore %rsp
      printf("    call " ASM_PRINTF "\n");
      printf("    mov %%rbx, %%rsp\n");
      break;
    case sIf:
    {
      // eval condition
      genExpression(statement->ifCondition, formals);
      // get label for if block
      int localLabel = labelCounter++;
      // if %rax is 0 go to ELSE label
      printf("    cmp $0, %%rax\n");
      printf("    je L%d_ELSE\n", localLabel);
      printf("L%d_THEN:\n", localLabel);
      // generate ifThen statement
      genStatement(statement->ifThen, formals);
      // jump to end
      printf("    jmp L%d_END\n", localLabel);
      // generate ifElse staatement
      printf("L%d_ELSE:\n", localLabel);
      genStatement(statement->ifElse, formals);
      printf("L%d_END:\n", localLabel);
      break;
    }
    case sWhile:
    {
      // get label for while loop
      int localLabel = labelCounter++;
      printf("L%d_BEGIN:\n", localLabel);
      // eval condition
      genExpression(statement->ifCondition, formals);
      // jump to end if condition is false
      printf("    cmp $0, %%rax\n");
      printf("    je L%d_END\n", localLabel);
      // generate body
      genStatement(statement->whileBody, formals);
      // jump to beginning
      printf("    jmp L%d_BEGIN\n", localLabel);
      printf("L%d_END:\n", localLabel);
      break;
    }
    case sBlock: {
      // generate each statement inside this block
      Block* cur = statement->block;
      while (cur != NULL)
      {
        genStatement(cur->first, formals);
        cur = cur->rest;
      }
      break;
    }
    case sReturn:
      // eval return value expression, saved to %rax
      genExpression(statement->returnValue, formals);
      // restore %rbp and %rsp
      printf("    mov %%rbp, %%rsp\n");
      printf("    pop %%rbp\n");
      printf("    ret\n");
      break;
  }
}

// evaluates the expressions used as actuals for a function call
void genActualsExpressions(Actuals* cur, Formals* formals, int counter)
{
  counter = 0;
  while (cur != NULL)
  {
    // eval expression
    genExpression(cur->first, formals);
    // store on stack
    printf("    mov %%rax, %d(%%rsp)\n", 8*counter);
    // move to next actual
    cur = cur->rest;
    counter++;
  }
}

void genExpression(Expression* exp, Formals* formals)
{
  switch (exp->kind)
  {
    case eVAR:
      // store variable value in %rax
      printf("    mov %s, %%rax\n", getVarLocation(exp->varName, formals));
      break;
    case eVAL:
      // store immediate value in %rax
#ifdef __APPLE__
      printf("    mov $%llu, %%rax\n", exp->val);
#else
      printf("    mov $%lu, %%rax\n", exp->val);
#endif
      break;
    case eCALL: {
      // determine how many actuals there are
      Actuals* cur = exp->callActuals;
      int total = 0;
      while (cur != NULL)
      {
        cur = cur->rest;
        total++;
      }
      // if there are actuals, allocate space on stack
      if (exp->callActuals != NULL)
        printf("    sub $%d, %%rsp\n", 8*total+8);
      // generate actual expressions
      genActualsExpressions(exp->callActuals, formals, total);
      // call function
      printf("    call fun_%s\n", exp->callName);
      // restore stack
      if (exp->callActuals != NULL)
        printf("    add $%d, %%rsp\n", 8*total+8);
      break;
    }
    default:
      // evaluate left side and save value
      genExpression(exp->left, formals);
      printf("    push %%rax\n");
      // evaluate right side
      genExpression(exp->right, formals);
      printf("    pop %%rcx\n");
      // add values
      if (exp->kind == ePLUS)
        printf("    add %%rcx, %%rax\n");
      // multiply values
      else if (exp->kind == eMUL)
        printf("    mul %%rcx\n");
      else
      {
        // compare and use appropriate flag to set %al
        printf("    cmp %%rax, %%rcx\n");
        if (exp->kind == eEQ)
        {
          printf("    sete %%al\n");
        }
        else if (exp->kind == eNE)
        {
          printf("    setne %%al\n");
        }
        else if (exp->kind == eLT)
        {
          printf("    setb %%al\n");
        }
        else if (exp->kind == eGT)
        {
          printf("    seta %%al\n");
        }
        // zero extend %al
        printf("    movzx %%al, %%rax\n");
      }
      break;
  }
}
int main(int argc, char *argv[]) {
  Funs *p = parse();
  // .text section
  printf("    .text\n");
  // generate functions
  genFuns(p);
  // main function calls fun_main
  printf("    .global main\n");
  printf("    .global _main\n");
  printf("main:\n");
  printf("_main:\n");
  printf("    push %%rbp\n");
  printf("    mov %%rsp, %%rbp\n");
  printf("    call fun_main\n");
  printf("    mov %%rbp, %%rsp\n");
  printf("    pop %%rbp\n");
  printf("    ret\n");
  // .data section
  printf(".data\n");
  printf("format_str:\n");
  printf("    .string \"%%lu\\n\"\n");
  // global variables go here
  exportVariables();
  return 0;
}

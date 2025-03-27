/*
 * File:	generator.cpp
 *
 * Description:	This file contains the public and member function
 *		definitions for the code generator for Simple C.
 *
 *		Extra functionality:
 *		- putting all the global declarations at the end
 *		- prefix and suffix for globals (required on some systems)
 */

# include <vector>
# include <cassert>
# include <iostream>
# include <map>
# include "generator.h"
# include "machine.h"
# include "Tree.h"
# include "string.h"

using namespace std;

static int offset;
static string funcname, tab = "\t";
static string suffix(Expression *expr);
static ostream &operator <<(ostream &ostr, Expression *expr);

static Register *rax = new Register("%rax", "%eax", "%al");
static Register *rbx = new Register("%rbx", "%ebx", "%bl");
static Register *rcx = new Register("%rcx", "%ecx", "%cl");
static Register *rdx = new Register("%rdx", "%edx", "%dl");
static Register *rsi = new Register("%rsi", "%esi", "%sil");
static Register *rdi = new Register("%rdi", "%edi", "%dil");
static Register *r8 = new Register("%r8", "%r8d", "%r8b");
static Register *r9 = new Register("%r9", "%r9d", "%r9b");
static Register *r10 = new Register("%r10", "%r10d", "%r10b");
static Register *r11 = new Register("%r11", "%r11d", "%r11b");
static Register *r12 = new Register("%r12", "%r12d", "%r12b");
static Register *r13 = new Register("%r13", "%r13d", "%r13b");
static Register *r14 = new Register("%r14", "%r14d", "%r14b");
static Register *r15 = new Register("%r15", "%r15d", "%r15b");

static vector<Register *> parameters = {rdi, rsi, rdx, rcx, r8, r9};
static vector<Register *> registers = {rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11};

static map<std::string, Label> strings; 
static std::vector<Label> labels;

static void assign(Expression *expr, Register *reg){

    if(expr != nullptr){
        if(expr->reg != nullptr)
            expr->reg->node = nullptr;
        
        expr->reg = reg;
    }

    if(reg != nullptr){
        if(reg->node != nullptr)
            reg->node->reg = nullptr;

        reg->node = expr;
    }
}



static void load(Expression *expr, Register *reg){
    if(reg->node != expr){
        
        //replaced assert with:
        if(reg->node != nullptr) {
            offset -= reg->node->type().size();
            reg->node->offset = offset;
            cout << "\tmov" << suffix(reg->node) << reg;
            cout << ", " << offset << "(%rbp)" << endl;
        }
        //...

        if(expr != nullptr) {
            unsigned size = expr->type().size();
            cout << "\tmov" << suffix(expr) << expr;
            cout << ", " << reg->name(size) << endl;
        }

        assign(expr, reg);
    }
}

static Register *getreg(){

    for (auto reg : registers) 
        if(reg->node == nullptr)
            return reg;

    load(nullptr, registers[0]);
    return registers[0];
}


/*
 * Function:	sign_extend_byte_arg (private)
 *
 * Description:	Sign extend a byte argument to 32 bits.  The Microsoft
 *		calling conventions explicitly state that parameters less
 *		than 64 bits long are not zero extended.  The System V
 *		conventions used for Unix-like systems do not specify what
 *		happens, but gcc and clang do sign extend, and clang
 *		apparently relies on it, but icc does not sign extend.
 *
 *		Writing to the 32 bit register will zero the upper 32-bits
 *		of the 64-bit register.  So in effect, an 8-bit value
 *		written to %al is sign extended into %eax but then zero
 *		extended into %rax.
 */

void sign_extend_byte_arg(Expression *arg)
{
    if (arg->type().size() == 1) {
	cout << tab << "movsbl" << tab << arg << ", ";
	cout << arg->reg->name(4) << endl;
    }
}


/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the given size.
 */

static string suffix(unsigned long size)
{
    return size == 1 ? "b\t" : (size == 4 ? "l\t" : "q\t");
}


/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the size of the
 *		given expression.
 */

static string suffix(Expression *expr)
{
    return suffix(expr->type().size());
}


/*
 * Function:	align (private)
 *
 * Description:	Return the number of bytes necessary to align the given
 *		offset on the stack.
 */

static int align(int offset)
{
    if (offset % STACK_ALIGNMENT == 0)
	return 0;

    return STACK_ALIGNMENT - (abs(offset) % STACK_ALIGNMENT);
}


/*
 * Function:	operator << (private)
 *
 * Description:	Convenience function for writing the operand of an
 *		expression using the output stream operator.
 */

static ostream &operator <<(ostream &ostr, Expression *expr)
{
    if (expr->reg != nullptr)
	return ostr << expr->reg;

    expr->operand(ostr);
    return ostr;
}


/*
 * Function:	Expression::operand
 *
 * Description:	Write an expression as an operand to the specified stream.
 */

void Expression::operand(ostream &ostr) const
{
    assert(offset != 0);
    ostr << offset << "(%rbp)";
}


/*
 * Function:	Identifier::operand
 *
 * Description:	Write an identifier as an operand to the specified stream.
 */

void Identifier::operand(ostream &ostr) const
{
    if (_symbol->offset == 0)
	ostr << global_prefix << _symbol->name() << global_suffix;
    else
	ostr << _symbol->offset << "(%rbp)";
}


/*
 * Function:	Number::operand
 *
 * Description:	Write a number as an operand to the specified stream.
 */

void Number::operand(ostream &ostr) const
{
    ostr << "$" << _value;
}


/*
 * Function:	Call::generate
 *
 * Description:	Generate code for a function call expression.
 *
 *		On a 64-bit platform, the stack needs to be aligned on a
 *		16-byte boundary.  So, if the stack will not be aligned
 *		after pushing any arguments, we first adjust the stack
 *		pointer.
 *
 *		Since all arguments are 8-bytes wide, we could simply do:
 *
 *		    if (args.size() > 6 && args.size() % 2 != 0)
 *			subq $8, %rsp
 */

void Call::generate()
{
    unsigned numBytes;


    /* Generate code for the arguments first. */ 

    numBytes = 0;

    for (int i = _args.size() - 1; i >= 0; i --)
	_args[i]->generate();


    /* Adjust the stack if necessary */

    if (_args.size() > NUM_PARAM_REGS) {
	numBytes = align((_args.size() - NUM_PARAM_REGS) * PARAM_ALIGNMENT);

	if (numBytes > 0)
	    cout << tab << "subq" << tab << "$" << numBytes << ", %rsp" << endl;
    }


    /* Move the arguments into the correct registers or memory locations. */

    for (int i = _args.size() - 1; i >= 0; i --) {
	if (i >= NUM_PARAM_REGS) {
	    numBytes += PARAM_ALIGNMENT;
	    load(_args[i], rax);
	    sign_extend_byte_arg(_args[i]);
	    cout << tab << "pushq" << tab << "%rax" << endl;

	} else {
	    load(_args[i], parameters[i]);
	    sign_extend_byte_arg(_args[i]);
	}

	assign(_args[i], nullptr);
    }


    /* Call the function and then reclaim the stack space.  We only need to
       assign the number of floating point arguments passed in vector
       registers to %eax if the function being called takes a variable
       number of arguments. */

    for (auto reg : registers)
	load(nullptr, reg);

    if (_id->type().parameters()->variadic)
	cout << tab << "movl" << tab << "$0, %eax" << endl;

    cout << tab << "call" << tab << global_prefix << _id->name() << endl;

    if (numBytes > 0)
	cout << tab << "addq" << tab << "$" << numBytes << ", %rsp" << endl;

    assign(this, rax);
}


/*
 * Function:	Block::generate
 *
 * Description:	Generate code for this block, which simply means we
 *		generate code for each statement within the block.
 */

void Block::generate()
{
    for (auto stmt : _stmts) {
	stmt->generate();

	for (auto reg : registers)
	    assert(reg->node == nullptr);
    }
}


/*
 * Function:	Simple::generate
 *
 * Description:	Generate code for a simple (expression) statement, which
 *		means simply generating code for the expression.
 */

void Simple::generate()
{
    _expr->generate();
    assign(_expr, nullptr);
}


/*
 * Function:	Function::generate
 *
 * Description:	Generate code for this function, which entails allocating
 *		space for local variables, then emitting our prologue, the
 *		body of the function, and the epilogue.
 */

void Function::generate()
{
    int param_offset;
    unsigned size;
    Symbols symbols;
    Types types;


    /* Assign offsets to the parameters and local variables. */

    param_offset = 2 * SIZEOF_REG;
    offset = param_offset;
    allocate(offset);


    /* Generate our prologue. */

    funcname = _id->name();
    cout << global_prefix << funcname << ":" << endl;
    cout << tab << "pushq" << tab << "%rbp" << endl;
    cout << tab << "movq" << tab << "%rsp, %rbp" << endl;
    cout << tab << "movl" << tab << "$" << funcname << ".size, %eax" << endl;
    cout << tab << "subq" << tab << "%rax, %rsp" << endl;


    /* Spill any parameters. */

    types = _id->type().parameters()->types;
    symbols = _body->declarations()->symbols();

    for (unsigned i = 0; i < NUM_PARAM_REGS; i ++)
	if (i < types.size()) {
	    size = symbols[i]->type().size();
	    cout << tab << "mov" << suffix(size) << parameters[i]->name(size);
	    cout << ", " << symbols[i]->offset << "(%rbp)" << endl;
	} else
	    break;


    /* Generate the body of this function. */

    _body->generate();


    /* Generate our epilogue. */

    cout << endl << global_prefix << funcname << ".exit:" << endl;
    cout << tab << "movq" << tab << "%rbp, %rsp" << endl;
    cout << tab << "popq" << tab << "%rbp" << endl;
    cout << tab << "ret" << endl << endl;

    offset -= align(offset - param_offset);
    cout << tab << ".set" << tab << funcname << ".size, " << -offset << endl;
    cout << tab << ".globl" << tab << global_prefix << funcname << endl << endl;
}


/*
 * Function:	generateGlobals
 *
 * Description:	Generate code for any global variable declarations.
 */

void generateGlobals(Scope *scope)
{
    const Symbols &symbols = scope->symbols();

    for (auto symbol : symbols)
	if (!symbol->type().isFunction()) {
	    cout << tab << ".comm" << tab << global_prefix << symbol->name();
	    cout << ", " << symbol->type().size() << endl;
	}

    cout << endl;

    cout << tab << ".data" << endl;

    
    for (auto str : strings){
        string printStr = str.first;
        cout << str.second << ":" << tab << ".asciz" << tab << "\"" << escapeString(printStr) << "\"" << endl;
    }

}


/*
 * Function:	Assignment::generate
 *
 * Description:	Generate code for an assignment statement.
 *
 *		
 */

void Assignment::generate()
{
    //assert(dynamic_cast<Number *>(_right));
    //assert(dynamic_cast<Identifier *>(_left));

    Expression *pointer;
    _right->generate();

    if(_left->isDereference(pointer)){
        
        pointer->generate();

        //by this point: code gen for right and child of left (pointer)

        if(pointer->reg == nullptr)
            load(pointer, getreg());
        
        if(_right->reg == nullptr)
            load(_right, getreg());

        //by this point, both loaded

        cout << tab << "mov" << suffix(_right) << _right << ", " << "(" << pointer << ")" << endl;
    
        assign(nullptr, _right->reg);
        assign(nullptr, pointer->reg);



    } else {
         
        if(_right->reg == nullptr)
            load(_right, getreg());

        cout << tab << "mov" << suffix(_left) << _right << ", " << _left << endl;

        assign(nullptr, _right->reg);
    }

}


void Add::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    //cout << "# add with types: " << _left->type() << ", " << _right->type() << endl;

    cout << "\tadd" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_right, nullptr);
    assign(this, _left->reg);

}


void Subtract::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tsub" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_right, nullptr);
    assign(this, _left->reg);
}


void Multiply::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\timul" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_right, nullptr);
    assign(this, _left->reg);
}


void Divide::generate(){

    _left->generate();
    _right->generate();

    load(_left, rax);
    load(nullptr, rdx);
    load(_right, rcx);

    if (_type.size() == 8) {
        cout << tab << "cqto" << endl;
    } else {
        cout << tab << "cltd" << endl;
    }
    

    cout << "\tidiv" << suffix(_right);
    cout << _right << endl;

    assign(nullptr, _left->reg);
    assign(nullptr, _right->reg);

    assign(this, rax);
}


void Remainder::generate(){

    _left->generate();
    _right->generate();

    load(_left, rax);
    load(nullptr, rdx);
    load(_right, rcx);

    if (_type.size() == 8) {
        cout << tab << "cqto" << endl;
    } else {
        cout << tab << "cltd" << endl;
    }
    

    cout << "\tidiv" << suffix(_right);
    cout << _right << endl;

    assign(nullptr, _left->reg);
    assign(nullptr, _right->reg);

    assign(this, rdx);
}


void LessThan::generate(){
    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "\tsetl" << tab << reg->byte() << endl;
    cout << "\tmovzbl" << tab << reg->byte() << ", " << this->reg << endl;

}

void GreaterThan::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "setg\t" << tab << reg->byte() << endl;
    cout << "movzbl\t" << tab << reg->byte() << ", " << this->reg << endl;
}

void LessOrEqual::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "setle\t" << tab << reg->byte() << endl;
    cout << "movzbl\t" << tab << reg->byte() << ", " << this->reg << endl;
}

void GreaterOrEqual::generate(){
    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "setge\t" << tab << reg->byte() << endl;
    cout << "movzbl\t" << tab << reg->byte() << ", " << this->reg << endl;
}

void Equal::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "\tsete" << tab << reg->byte() << endl;
    cout << "\tmovzbl" << tab << reg->byte() << ", " << this->reg << endl;
}

void NotEqual::generate(){

    _left->generate();
    _right->generate();

    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left);
    cout << _right << ", " << _left << endl;

    assign(_left, nullptr);
    assign(_right, nullptr);

    assign(this, getreg());

    cout << "setne\t" << tab << reg->byte() << endl;
    cout << "movzbl\t" << tab << reg->byte() << ", " << this->reg << endl;
}



void Not::generate(){
    _expr->generate();

    if(_expr->reg == nullptr)
        load(_expr, getreg());

    cout << "\tcmp" << suffix(_expr);
    cout << "$0" << ", " << _expr << endl;

    assign(_expr, nullptr);
    assign(this, getreg());

    cout << "sete\t" << tab << reg->byte() << endl;
    cout << "movzbl\t" << tab << reg->byte() << ", " << this->reg << endl;
}

void Negate::generate(){
    
    _expr->generate();

    if(_expr->reg == nullptr)
        load(_expr, getreg());

    cout << "\tneg" << suffix(_expr) << _expr << endl;

    assign(_expr, nullptr);
    assign(this, getreg());
}


void Expression::test(const Label &label, bool ifTrue){
    
    generate();

    if(reg == nullptr)
        load(this, getreg());
    
    cout << "\tcmp" << suffix(this) << "$0, " << this << endl;
    cout << (ifTrue ? "\tjne\t" : "\tje\t") << label << endl;

    assign(this, nullptr);
}


void While::generate(){
    Label loop, exit;
    labels.push_back(exit);

    cout << loop << ":" << endl;

    _expr->test(exit, false);
    _stmt->generate();

    cout << "\tjmp\t" << loop << endl;

    labels.pop_back();
    cout << exit << ":" << endl;
}


void Address::generate(){

    Expression *pointer;
    
    if(_expr->isDereference(pointer)){
        
        pointer->generate();

        if(pointer->reg == nullptr)
            load(pointer, getreg());

        assign(this, pointer->reg);

    } else {
        
        assign(this, getreg());

        _expr->generate();

        cout << "\tleaq\t" << _expr << ", " << this << endl;
    }
}

void Dereference::generate(){

    _expr->generate();

    if(_expr->reg == nullptr)
        load(_expr, getreg());

    assign(this, getreg());

    cout << "\tmov" << suffix(this) << "(" << _expr << "), " << this << endl;
    
    assign(_expr, nullptr);
}

void String::operand(ostream &ostr) const {

    if(strings.count(_value) == 0)
        strings[_value] = Label();
    
    ostr << strings[_value];
}

void Return::generate(){
    _expr->generate();

    load(_expr, rax);

    cout << "\tjmp\t" << funcname << ".exit" <<endl;

    assign(_expr, nullptr);
}

void Cast::generate() {
    
    //bug: when using in an add stmt, add reports invalid operands. both operands are long? 

    unsigned long source, target;

    source = _expr->type().size();
    target = _type.size();

    _expr->generate();

    if(_expr->reg == nullptr)
        load(_expr, getreg());

    if(source < target){
        if(source == 1){
            if(target == 4){
                //char to int

                cout << "\tmovsbl\t" << _expr << ", " << _expr->reg->name(target) << endl;

            } else {
                //char to long

                cout << "\tmovsbq\t" << _expr << ", " << _expr->reg->name(target) << endl;
            }

        } else {
            //int to long
            cout << "\tmovslq\t" << _expr << ", " << _expr->reg->name(target) << endl;
        }
    }

    assign(this, _expr->reg);
}



void LogicalOr::generate(){

    Label resultT = Label();
    Label resultF = Label();

    _left->generate();
    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left) << tab << "$0" << ", " << _left << endl;

    cout << "\tjne\t" << resultT << endl;

    _right->generate();
    if(_right->reg == nullptr)
        load(_right, getreg());

    cout << "\tcmp" << suffix(_right) << tab << "$0" << ", " << _right << endl;

    cout << "\tjne\t" << resultT << endl;

    assign(this, getreg());

    cout << "\tmov" << suffix(this) << tab <<  "$0, " << this->reg << endl;
    cout << "\tjmp\t" << resultF << endl;

    cout << resultT << ":" << endl;

    cout << "\tmov" << suffix(this) << tab <<  "$1, " << this->reg << endl;

    cout << resultF << ":" << endl;

}

void LogicalAnd::generate(){

    Label resultT = Label();
    Label resultF = Label();

    _left->generate();
    if(_left->reg == nullptr)
        load(_left, getreg());

    cout << "\tcmp" << suffix(_left) << tab << "$0" << ", " << _left << endl;

    cout << "\tje\t" << resultT << endl;

    _right->generate();
    if(_right->reg == nullptr)
        load(_right, getreg());

    cout << "\tcmp" << suffix(_right) << tab << "$0" << ", " << _right << endl;

    cout << "\tje\t" << resultT << endl;

    assign(this, getreg());

    cout << "\tmov" << suffix(this) << tab <<  "$1, " << this->reg << endl;
    cout << "\tjmp\t" << resultF << endl;

    cout << resultT << ":" << endl;

    cout << "\tmov" << suffix(this) << tab <<  "$0, " << this->reg << endl;

    cout << resultF << ":" << endl;

}

void For::generate(){
    
    _init->generate();

    Label loop = Label();
    Label exit = Label();

    labels.push_back(exit);

    cout << loop << ":" << endl;

    _expr->generate();
    if(_expr->reg == nullptr)
        load(_expr, getreg());

    cout << "\tcmp" << suffix(_expr) << tab << "$0" << ", " << _expr << endl;
    cout << "\tje\t" << exit << endl;

    _stmt->generate();
    _incr->generate();

    cout << "\tjmp\t" << loop << endl;

    labels.pop_back();
    cout << exit << ":" << endl;
}


void If::generate(){

    Label skip = Label();
    Label exit = Label();

    _expr->generate();
    if(_expr->reg == nullptr)
        load(_expr, getreg());

    cout << "\tcmp" << suffix(_expr) << tab << "$0" << ", " << _expr << endl;
    cout << "\tje\t" << skip << endl;

    _thenStmt->generate();

    if (_elseStmt == nullptr) {
        
        cout << skip << ":" << endl;

    } else {
        
        cout << "\tjmp\t" << exit << endl;
        cout << skip << ":" << endl;
        _elseStmt->generate();
        cout << exit << ":" << endl;
    
    }

}

void Break::generate(){

    cout << "\tjmp\t" << labels.back() << endl;
    labels.pop_back();
}

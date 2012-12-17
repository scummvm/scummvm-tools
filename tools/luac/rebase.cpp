#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "luac.h"
#include "tools/lua/lmem.h"
#include "tools/lua/lstring.h"

static const Opcode Info[]=			/* ORDER lopcodes.h */
{
#include "opcode.h"
};

int increase_const_list(TProtoFunc* func);
void uniform_const_list(TProtoFunc* func, TProtoFunc* base);
void rec_bytecode(TProtoFunc* func, int* inst);
int num_opcodes(TProtoFunc* tf);
void fix_op(Opcode *op);
bool cmp(const TObject *a, const TObject *b);

/*Rebase a function upon another to reduce the differences
 The main requiment is that the functions order still unchanged
 Don't delete useless functions and add new functions as last, or in
 another file*/
void rebase(TProtoFunc* func, TProtoFunc* base) {
	//Set the right filename and line number
	//luaS_free(func->fileName);
	func->lineDefined = base->lineDefined;
	func->fileName = base->fileName;

	//Uniformize the const list in order to reduce the differences
	uniform_const_list(func, base);

	//Recursively rebase nested functions
	//It assumes that the subfunctions has the same index
	for (int j = 0; j < func->nconsts; ++j)
		if (ttype(&func->consts[j]) == LUA_T_PROTO && ttype(&base->consts[j]) == LUA_T_PROTO)
			rebase(tfvalue(&func->consts[j]), tfvalue(&base->consts[j]));
}

void uniform_const_list(TProtoFunc* func, TProtoFunc* base) {
	int i, j, k, j0;
	j0 = 0;

	//Part1: Add back the deleted constant into the new function
	int max_const = func->nconsts;
	for (i = 0; i < base->nconsts; ++i) {
		//Ignore functions
		if (ttype(&base->consts[i]) == LUA_T_PROTO)
			continue;

		for (j = j0; j < func->nconsts; ++j)
			if (cmp(&base->consts[i], &func->consts[j])) {
				j0 = j + 1;

				//Set the value of numbers to the value of base functions
				//since they could be sligthly differents (rounding errors)
				if (ttype(&base->consts[i]) == LUA_T_NUMBER)
					nvalue(&func->consts[j]) = nvalue(&base->consts[i]);
				break;
			}

		if (j == func->nconsts) { //Const not found, re-add it
			if (func->nconsts + 1 >= max_const)
				max_const = increase_const_list(func);
			++func->nconsts;
			ttype(&func->consts[func->nconsts - 1]) = ttype(&base->consts[i]);
			assert(ttype(&base->consts[i]) == LUA_T_NUMBER || ttype(&base->consts[i]) == LUA_T_STRING);
			if (ttype(&base->consts[i]) == LUA_T_NUMBER)
				nvalue(&func->consts[func->nconsts - 1]) = nvalue(&base->consts[i]);
			else if (ttype(&base->consts[i]) == LUA_T_STRING)
				tsvalue(&func->consts[func->nconsts - 1]) = tsvalue(&base->consts[i]);
		}
	}

	//Part2: Scan the const list and search the right position by comparing the
	//current element with the const list of base function
	int *inst;
	TObject *new_const_list;
	bool *already_used;

	inst = luaM_newvector(func->nconsts, int);
	new_const_list = luaM_newvector(func->nconsts, TObject);
	already_used = luaM_newvector(base->nconsts, bool);
	for (i = 0; i < base->nconsts; ++i)
		already_used[i] = false;

	k = func->nconsts;
	assert(func->nconsts >= base->nconsts);
	for (j = 0; j < func->nconsts; ++j) {
		for (i = 0; i < base->nconsts; ++i)
			if (!already_used[i] && cmp(&base->consts[i], &func->consts[j]))
				break;

		if (i != base->nconsts) {
			inst[j] = i;
			already_used[i] = true;
		} else
			inst[j] = --k;	//if there is a new element, put it in the end of the list

		assert(inst[j] < func->nconsts);
		new_const_list[inst[j]] = func->consts[j];
	}
	luaM_free(func->consts);
	luaM_free(already_used);
	func->consts = new_const_list;

	//Part3: Modify bytecode in order to use new constant list
	rec_bytecode(func, inst);
	luaM_free(inst);
}

void rec_bytecode(TProtoFunc* func, int* inst) {
	int n_op = num_opcodes(func);
	int i, newsize;
	Opcode *opcode_list = luaM_newvector(n_op, Opcode);

	Byte* p = func->code;

	//Change const index
	i = 0;
	newsize = 0;
	while (1) {
		p += INFO(func, p, &opcode_list[i]);
		Opcode &op = opcode_list[i];

		//Change const index, if needed
		if (op.op_class == PUSHCONSTANT ||
			op.op_class == GETGLOBAL ||
			op.op_class == SETGLOBAL ||
			op.op_class == GETDOTTED ||
			op.op_class == PUSHSELF)
			if (op.arg != inst[op.arg]) {
				op.arg = inst[op.arg];
				fix_op(&op);
			}

		newsize += op.size;
		++i;
		if (op.op == ENDCODE)
			break;
	}

	luaM_free(func->code);
	Byte *code = (Byte*)luaM_malloc(newsize);
	func->code = code;

	//Compile bytecode
	Byte out[4];

	//Out stacksize and arguments number
	code[0] = (byte)opcode_list[0].arg;
	if (opcode_list[1].op == VARARGS)
		code[1] = (byte)opcode_list[1].arg + ZEROVARARG;
	else
		code[1] = (byte)opcode_list[1].arg;
	code += 2;

	for (i = 2; i < n_op; ++i) {
		Opcode &op = opcode_list[i];

		//Out opcode
		out[0] = (byte)op.op;

		//Out args
		if (op.op == SETLIST || op.op == CLOSURE || op.op == CALLFUNC) {
			out[1] = (byte)op.arg;
			out[2] = (byte)op.arg2;
		}
		else if (op.size == 2)
			out[1] = (byte)op.arg;
		else if (op.size >= 3)
			WRITE_LE_UINT16(out + 1, op.arg);
		if (op.op == SETLISTW)
			out[3] = (byte)op.arg2;

		memcpy(code, out, op.size);
		code += op.size;
	}

	luaM_free(opcode_list);
}

//Works only on opcodes that access to constant list
//Don't use with others opcodes!
void fix_op(Opcode *op) {
	//Fix opcode
	if (op->arg < 8)						//built-in paramter
		op->op = op->op_class + op->arg + 1;
	else if (op->arg < 256)					//byte parameter
		op->op = op->op_class;
	else									//word parameter
		op->op = op->op_class + 9;

	//Fix size and name
	op->size = Info[op->op].size;
	op->name = Info[op->op].name;
}

int num_opcodes(TProtoFunc* tf) {
	Byte* code = tf->code;
	Byte* p = code;
	int i = 0;
	while (1) {
		Opcode OP;
		p+=INFO(tf, p, &OP);
		++i;
	if (OP.op == ENDCODE)
		break;
	}

	return i;
}

bool cmp(const TObject *a, const TObject *b) {
	if (ttype(a) != ttype(b))
		return false;

	if (ttype(a) == LUA_T_PROTO)
		return true;

	if (ttype(a) == LUA_T_NUMBER) {
		//A simple fp comparision
		double diff, fa, fb, largest;

		diff = fabs(nvalue(a) - nvalue(b));
		fa = fabs(nvalue(a));
		fb = fabs(nvalue(b));
		largest = (fb > fa) ? fb : fa;

		if (diff <= largest * 1e-5)		//Ten parts in a million should be enough
			return true;
		else
			return false;
	}

	if (ttype(a) == LUA_T_STRING)
		if (strncmp(svalue(a), svalue(b), tsvalue(a)->u.s.len) == 0)
			return true;

	return false;
}

//Increase by one the const list count
int increase_const_list(TProtoFunc* func) {
	return luaM_growvector(&func->consts, func->nconsts, TObject,
                                    constantEM, MAX_WORD);
}

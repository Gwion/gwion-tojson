#include "gwion_util.h"
#include "gwion_ast.h"
#include "tojson.h"

#define SCOPE(a, b) \
  do { a->scope++; b; a->scope--; } while(0)

#define NEXT(a, b) \
  do { tojson_next(a); b; } while(0)

#define ARRAY(a, b, c) \
  do { tojson(a, "\"%s\":[", b); a->mark=false; c; tojson(a, "]"); } while(0)

#define NODE(a, b, c) \
  do { const bool mark = a->mark; a->mark=false; tojson(a, "{\"node\":\"%s\"", b); c; tojson(a, "}"); a->mark = mark; } while(0)

ANN static inline void tojson(ToJson *a, const m_str fmt, ...) {
  if(a->scope)
    return;
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
  a->mark = true;
}

ANN static inline void tojson_next(ToJson *a) {
  if(a->mark)
    printf(",");
}

ANN static inline void tojson_add(GwText *text, m_str data) {
  while(*data == ' ')
    data++;
  if(text->str)
    text_add(text, " ");
  text_add(text, data);
}

ANN static inline void tojson_up(ToJson *a, const m_str data) {
  GwText *const text = (GwText*)vector_back(&a->ctx);
  tojson_add(text, data);
}

ANN static inline void tojson_down(ToJson *a, const m_str data) {
  tojson_add(&a->next, data);
}

ANN static void tojson_pending(ToJson *a) {
  if(a->next.str && *a->next.str != '\0') {
    tojson(a, "\"description\":\"%s\"", a->next.str);
    text_reset(&a->next);
  }
}

ANN static void tojson_push(ToJson *a) {
  GwText *const text = new_text(a->mp);
  vector_add(&a->ctx, (m_uint)text);
  if(a->next.str && *a->next.str != '\0') {
    tojson_add(text, a->next.str);
    text_reset(&a->next);
  }
}

ANN static void tojson_pop(ToJson *a) {
  GwText *const text = (GwText*)vector_pop(&a->ctx);
  if(text->str)
    NEXT(a, tojson(a, "\"description\":\"%s\"", text->str));
  free_text(text);
}

ANN static void tojson_symbol(ToJson *a, Symbol b) {
  tojson(a, "\"name\":\"%s\"", s_name(b));
}

ANN static void tojson_array_sub(ToJson *a, Array_Sub b) {
  ARRAY(a, "array",
    if(b->exp)
      NEXT(a, tojson_exp(a, b->exp));
  );
}

ANN static void tojson_id_list(ToJson *a, ID_List b) {
  tojson(a, "\"%s\"", s_name(b->xid));
  if(b->next)
    NEXT(a, tojson_id_list(a, b->next));
}

ANN static void _tojson_type_list(ToJson *a, Type_List b) {
  tojson_type_decl(a, b->td);
  if(b->next)
    NEXT(a, tojson_type_list(a, b->next));
}

ANN static void tojson_type_list(ToJson *a, Type_List b) {
  ARRAY(a, "template", _tojson_type_list(a, b));
}

ANN static void tojson_tmpl(ToJson *a, Tmpl *b) {
  if(b->list)
    ARRAY(a, "template", tojson_id_list(a, b->list));
  if(b->call)
    tojson_type_list(a, b->call);
}

ANN static void tojson_range(ToJson *a, Range *b) {
  if(b->start)
    ARRAY(a, "start", tojson_exp(a, b->start));
  if(b->end)
    ARRAY(a, "end", tojson_exp(a, b->end));
}

#define FLAG(a,b) ((a) & (b)) == (b))

#define AEFLAG(a,b) FLAG(a, ae_flag_##b)
#define TOJSON_AEFLAG(a,b,c) if(AEFLAG(b, c) NEXT(a, tojson(a, "\"%s\"", #c))
ANN static void tojson_flag(ToJson *a, ae_flag b) {
  ARRAY(a, "flag",
    TOJSON_AEFLAG(a, b, static);
    TOJSON_AEFLAG(a, b, private);
    TOJSON_AEFLAG(a, b, global);
    TOJSON_AEFLAG(a, b, const);
    TOJSON_AEFLAG(a, b, late);
    TOJSON_AEFLAG(a, b, abstract);
    TOJSON_AEFLAG(a, b, final);
    TOJSON_AEFLAG(a, b, protect);
  );
}
#undef TOJSON_AEFLAG
#undef AEFLAG

ANN static void tojson_type_decl(ToJson *a, Type_Decl *b) {
  tojson(a, "\"type\":{");
  tojson_symbol(a, b->xid);
  NEXT(a, tojson_flag(a, b->flag));
  if(b->ref)
    NEXT(a, tojson(a, "\"reference\":%u", b->ref));
  if(b->option)
    NEXT(a, tojson(a, "\"option\":%u", b->option));
  if(b->array)
    NEXT(a, tojson_array_sub(a, b->array));
  if(b->types)
    NEXT(a, tojson_type_list(a, b->types));
  if(b->next) {
    NEXT(a, tojson(a, "\"next\":{");
      tojson_type_decl(a, b->next);
      tojson(a, "}");
    );
  }
  tojson(a, "}");
}

ANN static void tojson_prim_id(ToJson *a, Symbol *b) {
  tojson_symbol(a, *b);
}

ANN static void tojson_prim_num(ToJson *a, m_uint *b) {
    tojson(a, "\"num\":%lu", *b);
}

ANN static void tojson_prim_float(ToJson *a, m_float *b) {
    tojson(a, "\"float\":%f", *b);
}

ANN static void tojson_prim_str(ToJson *a, m_str *b) {
    tojson(a, "\"string\":\"%s\"", *b);
}

ANN static void tojson_prim_array(ToJson *a, Array_Sub *b) {
  tojson_array_sub(a, *b);
}

ANN static void tojson_prim_range(ToJson *a, Range* *b) {
  tojson_range(a, *b);
}

ANN static void tojson_prim_hack(ToJson *a, Exp *b) {
  NODE(a, "pretty_print", tojson_exp(a, *b));
}

ANN static void tojson_prim_typeof(ToJson *a, Exp *b) {
  NODE(a, "typeof", tojson_exp(a, *b));
}

ANN static void tojson_prim_interp(ToJson *a, Exp *b) {
  NODE(a, "interp", tojson_exp(a, *b));
}

ANN static void tojson_prim_char(ToJson *a, m_str *b) {
  tojson(a, "\"char\":\"%s\"", *b);
}

ANN static void tojson_prim_nil(ToJson *a, void *b) {
  tojson(a, "\"nil\":\"()\"");
}

ANN static void tojson_prim_perform(ToJson *a, Symbol b) {
  tojson(a, "\"perform\":\"%s\"", s_name(b));
}

DECL_PRIM_FUNC(tojson, void, ToJson*)
ANN static void tojson_prim(ToJson *a, Exp_Primary *b) {
  tojson_prim_func[b->prim_type](a, &b->d);
}

ANN static void tojson_var_decl(ToJson *a, Var_Decl b) {
  if(b->xid)
    tojson(a, "\"%s\"", s_name(b->xid));
  if(b->array)
    NEXT(a, tojson_array_sub(a, b->array));
}

ANN static void tojson_var_decl_list(ToJson *a, Var_Decl_List b) {
  tojson_var_decl(a, b->self);
  if(b->next)
    NEXT(a, tojson_var_decl_list(a, b->next));
}

ANN static void tojson_exp_decl(ToJson *a, Exp_Decl *b) {
    if(b->td)
      tojson_type_decl(a, b->td);
    NEXT(a, ARRAY(a, "data", tojson_var_decl_list(a, b->list)));
    NEXT(a, tojson_pending(a));
}

ANN static void tojson_exp_binary(ToJson *a, Exp_Binary *b) {
    ARRAY(a, "lhs", tojson_exp(a, b->lhs));
    NEXT(a, tojson(a, "\"op\":\"%s\"", s_name(b->op)));
    NEXT(a, ARRAY(a, "rhs", tojson_exp(a, b->rhs)));
}

ANN static void tojson_exp_unary(ToJson *a, Exp_Unary *b) {
    tojson(a, "\"op\":\"%s\"", s_name(b->op));
    switch(b->unary_type) {
      case unary_exp:
    if(b->exp)
      NEXT(a, ARRAY(a, "exp", tojson_exp(a, b->exp)));
      break;
      case unary_td:
    if(b->td)
      NEXT(a, tojson_type_decl(a, b->td));
       break;
      case unary_code:
    if(b->code)
      NEXT(a, tojson_stmt(a, b->code));
      break;
    }
}

ANN static void tojson_exp_cast(ToJson *a, Exp_Cast *b) {
  tojson_type_decl(a, b->td);
  NEXT(a, ARRAY(a, "exp", tojson_exp(a, b->exp)));
}

ANN static void tojson_exp_post(ToJson *a, Exp_Postfix *b) {
  tojson(a, "\"op\":\"%s\"", s_name(b->op));
  NEXT(a, ARRAY(a, "exp", tojson_exp(a, b->exp)));
}

ANN static void tojson_exp_call(ToJson *a, Exp_Call *b) {
  NEXT(a, ARRAY(a, "func", tojson_exp(a, b->func)));
  if(b->args)
    NEXT(a, ARRAY(a, "arguments", tojson_exp(a, b->args)));
  if(b->tmpl)
    NEXT(a, tojson_tmpl(a, b->tmpl));
}

ANN static void tojson_exp_array(ToJson *a, Exp_Array *b) {
  tojson_exp(a, b->base);
  NEXT(a, tojson_array_sub(a, b->array));
}

ANN static void tojson_exp_slice(ToJson *a, Exp_Slice *b) {
  NEXT(a, tojson_range(a, b->range));
}

ANN static void tojson_exp_if(ToJson *a, Exp_If *b) {
  tojson_exp(a, b->cond);
  if(b->if_exp)
    tojson_exp(a, b->if_exp);
  tojson_exp(a, b->else_exp);
}

ANN static void tojson_exp_dot(ToJson *a, Exp_Dot *b) {
  tojson_exp(a, b->base);
  tojson_symbol(a, b->xid);
}

ANN static void tojson_exp_lambda(ToJson *a, Exp_Lambda *b) {
  tojson_func_def(a, b->def);
}

ANN static void tojson_exp_td(ToJson *a, Type_Decl *b) {
  tojson_type_decl(a, b);
}

#define EXPTYPE(a, b) case ae_exp_##b: tojson(a, "\"%s\"", #b); break
ANN static void tojson_exp_type(ToJson *a, ae_exp_t b) {
  tojson(a, "\"exp_type\":");
  switch(b) {
    EXPTYPE(a, decl);
    EXPTYPE(a, binary);
    EXPTYPE(a, unary);
    EXPTYPE(a, primary);
    EXPTYPE(a, cast);
    EXPTYPE(a, post);
    EXPTYPE(a, call);
    EXPTYPE(a, array);
    EXPTYPE(a, slice);
    EXPTYPE(a, if);
    EXPTYPE(a, dot);
    EXPTYPE(a, lambda);
    EXPTYPE(a, td);
  }
}

DECL_EXP_FUNC(tojson, void, ToJson*)
ANN static void tojson_exp(ToJson *a, Exp b) {
  NODE(a, "exp",
    NEXT(a, tojson_exp_type(a, b->exp_type));
    NEXT(a, tojson_exp_func[b->exp_type](a, &b->d));
  );
  if(b->next)
    NEXT(a, tojson_exp(a, b->next));
}

ANN static void tojson_stmt_exp(ToJson *a, Stmt_Exp b) {
  if(b->val)
    tojson_exp(a, b->val);
}

ANN static void tojson_stmt_while(ToJson *a, Stmt_Flow b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_until(ToJson *a, Stmt_Flow b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_for(ToJson *a, Stmt_For b) {
  SCOPE(a,
    tojson_stmt(a, b->c1);
    if(b->c2)
      tojson_stmt(a, b->c2);
    if(b->c3)
      tojson_exp(a, b->c3);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_each(ToJson *a, Stmt_Each b) {
  SCOPE(a,
    tojson_symbol(a, b->sym);
    tojson_exp(a, b->exp);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_loop(ToJson *a, Stmt_Loop b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_if(ToJson *a, Stmt_If b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_stmt(a, b->if_body);
    if(b->else_body)
      tojson_stmt(a, b->else_body);
  );
}

ANN static void tojson_stmt_code(ToJson *a, Stmt_Code b) {
  SCOPE(a,
    if(b->stmt_list)
      tojson_stmt_list(a, b->stmt_list);
  );
}

ANN static void tojson_stmt_varloop(ToJson *a, Stmt_VarLoop b) {
  SCOPE(a,
    tojson_exp(a, b->exp);
    tojson_stmt(a, b->body);
  );
}

ANN static void tojson_stmt_break(ToJson *a, Stmt_Exp b) {
}

ANN static void tojson_stmt_continue(ToJson *a, Stmt_Exp b) {
}

ANN static void tojson_stmt_return(ToJson *a, Stmt_Exp b) {
  if(b->val)
    tojson_exp(a, b->val);
}

ANN static void tojson_case_list(ToJson *a, Stmt_List b) {
    tojson_stmt_case(a, &b->stmt->d.stmt_match);
    if(b->next)
      tojson_case_list(a, b->next);
}

ANN static void tojson_stmt_match(ToJson *a, Stmt_Match b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_case_list(a, b->list);
    if(b->where)
      tojson_stmt(a, b->where);
  );
}

ANN static void tojson_stmt_case(ToJson *a, Stmt_Match b) {
  SCOPE(a,
    tojson_exp(a, b->cond);
    tojson_stmt_list(a, b->list);
    if(b->when)
      tojson_exp(a, b->when);
  );
}

ANN static void tojson_stmt_index(ToJson *a, Stmt_Index b) {
}

ANN static void tojson_stmt_pp(ToJson *a, Stmt_PP b) {
  if(b->data && b->pp_type == ae_pp_comment) {
  const m_str text = b->data;
  if(text[0] == '-')
    tojson_down(a, b->data + 1);
  else if(text[0] == '+')
    tojson_up(a, b->data + 1);
  }
  tojson(a, "{}");
}

ANN static void tojson_stmt_retry(ToJson *a, Stmt_Exp b) {
}

ANN static void tojson_stmt_try(ToJson *a, Stmt_Try b) {
  SCOPE(a, tojson_stmt(a, b->stmt));
}

ANN static void tojson_stmt_defer(ToJson *a, Stmt_Defer b) {
  SCOPE(a, tojson_stmt(a, b->stmt));
}

DECL_STMT_FUNC(tojson, void, ToJson*)
ANN static void tojson_stmt(ToJson *a, Stmt b) {
  tojson_stmt_func[b->stmt_type](a, &b->d);
}

ANN static void tojson_arg_list(ToJson *a, Arg_List b) {
  NODE(a, "argument", 
    if(b->td)
      NEXT(a, tojson_type_decl(a, b->td));
    NEXT(a, tojson_var_decl(a, b->var_decl));
    if(b->next)
      NEXT(a, tojson_arg_list(a, b->next));
  );
}

ANN static void tojson_union_list(ToJson *a, Union_List b) {
  NODE(a, "union_member",
    NEXT(a, tojson_type_decl(a, b->td));
    NEXT(a, tojson_symbol(a, b->xid));
  );
}

ANN static void tojson_stmt_list(ToJson *a, Stmt_List b) {
  tojson_stmt(a, b->stmt);
  if(b->next)
    NEXT(a, tojson_stmt_list(a, b->next));
}

ANN static void tojson_effects(ToJson *a, Vector b) {
  tojson_symbol(a, (Symbol)vector_front(b));
  for(m_uint i = 1; i < vector_size(b); i++)
    tojson_symbol(a, (Symbol)vector_at(b, i));
}

#define FBFLAG(a,b) FLAG(a, fbflag_##b)
#define TOJSON_FBFLAG(a,b,c) if(FBFLAG(b, c) NEXT(a, tojson(a, "\"%s\"", #c))
ANN static void tojson_fbflag(ToJson *a, enum fbflag b) {
  ARRAY(a, "funcflag",
    TOJSON_FBFLAG(a, b, op);
    TOJSON_FBFLAG(a, b, unary);
    TOJSON_FBFLAG(a, b, postfix);
    TOJSON_FBFLAG(a, b, variadic);
    TOJSON_FBFLAG(a, b, internal);
    TOJSON_FBFLAG(a, b, lambda);
  );
}
#undef TOJSON_FBFLAG
#undef FBFLAG

ANN static void tojson_func_base(ToJson *a, Func_Base *b) {
  tojson_push(a);
  if(b->td)
    NEXT(a, tojson_type_decl(a, b->td));
  NEXT(a, tojson_symbol(a, b->xid));
  NEXT(a, tojson_flag(a, b->flag));
  NEXT(a, tojson_fbflag(a, b->fbflag));
  if(b->args)
    NEXT(a, ARRAY(a, "arguments", tojson_arg_list(a, b->args)));
  if(b->tmpl)
    NEXT(a, tojson_tmpl(a, b->tmpl));
  if(b->effects.ptr)
    NEXT(a, tojson_effects(a, &b->effects));
  tojson_pop(a);
}

ANN static void tojson_func_def(ToJson *a, Func_Def b) {
  NODE(a, "function",
    tojson_func_base(a, b->base);
    if(b->d.code)
      tojson_stmt(a, b->d.code);
  );
}
/*
#define CFLAG(a,b) FLAG(a, cflag_##b)
#define TOJSON_CFLAG(a,b,c) if(CFLAG(b, c) NEXT(a, tojson(a, "\"%s\"", #c))
ANN static void tojson_cflag(ToJson *a, enum cflag b) {
  ARRAY(a, "classflag",
    TOJSON_CFLAG(a, b, none);
    TOJSON_CFLAG(a, b, struct);
    TOJSON_CFLAG(a, b, trait);
  );
}
#undef TOJSON_CFLAG
#undef cFLAG
*/
ANN static void tojson_class_def(ToJson *a, Class_Def b) {
  if(!cflag(b, cflag_struct))
    tojson(a, "{\"node\":\"class\"");
  else
    tojson(a, "{\"node\":\"struct\"");
//  NEXT(a, tojson_cflag(a, b->cflag));
  NEXT(a, tojson_type_def(a, &b->base));
  NEXT(a, tojson_flag(a, b->flag));
  if(b->body)
    NEXT(a, ARRAY(a, "data", tojson_ast(a, b->body)));
  tojson(a, "}");
}

ANN static void tojson_enum_def(ToJson *a, Enum_Def b) {
  NODE(a, "enum",
    tojson_push(a);
    NEXT(a, tojson_flag(a, b->flag));
    tojson_id_list(a, b->list);
    if(b->xid)
      tojson_symbol(a, b->xid);
    tojson_pop(a);
  );
}

ANN static void tojson_union_def(ToJson *a, Union_Def b) {
  NODE(a, "union",
    tojson_push(a);
    NEXT(a, tojson_symbol(a, b->xid));
    NEXT(a, tojson_flag(a, b->flag));
    if(b->tmpl)
      NEXT(a, tojson_tmpl(a, b->tmpl));
    NEXT(a, ARRAY(a, "data", tojson_union_list(a, b->l)));
    tojson_pop(a);
  );
}

ANN static void tojson_fptr_def(ToJson *a, Fptr_Def b) {
  NODE(a, "funptr", tojson_func_base(a, b->base));
}

ANN static void tojson_type_def(ToJson *a, Type_Def b) {
  tojson_push(a);
  if(b->ext)
    tojson_type_decl(a, b->ext);
  tojson_symbol(a, b->xid);
  if(b->tmpl)
    NEXT(a, tojson_tmpl(a, b->tmpl));
 tojson_pop(a);
}

ANN static void tojson_extend_def(ToJson *a, Extend_Def b) {
  NODE(a, "extends",
    tojson_ast(a, b->body);
    tojson_type_decl(a, b->td);
  );
}

DECL_SECTION_FUNC(tojson, void, ToJson*)
ANN static void tojson_section(ToJson *a, Section *b) {
  tojson_section_func[b->section_type](a, *(void**)&b->d);
}

ANN static void tojson_ast(ToJson *a, Ast b) {
  tojson_section(a, b->section);
  if(b->next) {
    NEXT(a, tojson_ast(a, b->next));
    if(!a->mark)
      tojson(a, "{}");
  }
}

ANN static void tojson_run(ToJson *a, Ast b, const Symbol name) {
  NODE(a, "title",
    tojson_push(a);
    NEXT(a, ARRAY(a, "data", tojson_ast(a, b)));
    tojson_pop(a);
  );
}

int main(int argc, char **argv) {
  MemPool mp = mempool_ini(sizeof(struct Exp_));
  SymTable* st = new_symbol_table(mp, 65347);
  struct PPArg_ ppa = { .lint=1 };
  ToJson tojson = { .mp=mp, .mark=false };
  vector_init(&tojson.ctx);
  tojson.next.mp = mp;
  pparg_ini(mp, &ppa);
  for(int i = 1; i < argc; ++i) {
    FILE* file = fopen(argv[i], "r");
    if(!file)
      continue;
    struct AstGetter_ arg = { argv[i], file, st , .ppa=&ppa };
    const Ast ast = parse(&arg);
    if(ast) {
      tojson_run(&tojson, ast, insert_symbol(st, argv[i]));
      free_ast(mp, ast);
      assert(!vector_size(&tojson.ctx));
    }
    fclose(file);
  }
  vector_release(&tojson.ctx);
  pparg_end(&ppa);
  free_symbols(st);
  mempool_end(mp);
}

typedef struct ToJson_ {
  GwText next;
  MemPool mp;
  struct Vector_ ctx;
  uint scope;
  bool mark;
} ToJson;

ANN static void tojson_symbol(ToJson *a, Symbol b);
ANN static void tojson_array_sub(ToJson *a, Array_Sub b);
ANN static void tojson_id_list(ToJson *a, ID_List b);
ANN static void tojson_type_list(ToJson *a, Type_List b);
ANN static void tojson_tmpl(ToJson *a, Tmpl *b);
ANN static void tojson_range(ToJson *a, Range *b);
ANN static void tojson_type_decl(ToJson *a, Type_Decl *b);
ANN static void tojson_prim_id(ToJson *a, Symbol *b);
ANN static void tojson_prim_num(ToJson *a, m_uint *b);
ANN static void tojson_prim_float(ToJson *a, m_float *b);
ANN static void tojson_prim_str(ToJson *a, m_str *b);
ANN static void tojson_prim_array(ToJson *a, Array_Sub *b);
ANN static void tojson_prim_range(ToJson *a, Range* *b);
ANN static void tojson_prim_hack(ToJson *a, Exp *b);
ANN static void tojson_prim_typeof(ToJson *a, Exp *b);
ANN static void tojson_prim_interp(ToJson *a, Exp *b);
ANN static void tojson_prim_char(ToJson *a, m_str *b);
ANN static void tojson_prim_nil(ToJson *a, void *b);
ANN static void tojson_prim_perform(ToJson *a, Symbol b);
ANN static void tojson_prim(ToJson *a, Exp_Primary *b);
ANN static void tojson_var_decl(ToJson *a, Var_Decl b);
ANN static void tojson_var_decl_list(ToJson *a, Var_Decl_List b);
ANN static void tojson_exp_decl(ToJson *a, Exp_Decl *b);
ANN static void tojson_exp_binary(ToJson *a, Exp_Binary *b);
ANN static void tojson_exp_unary(ToJson *a, Exp_Unary *b);
ANN static void tojson_exp_cast(ToJson *a, Exp_Cast *b);
ANN static void tojson_exp_post(ToJson *a, Exp_Postfix *b);
ANN static void tojson_exp_call(ToJson *a, Exp_Call *b);
ANN static void tojson_exp_array(ToJson *a, Exp_Array *b);
ANN static void tojson_exp_slice(ToJson *a, Exp_Slice *b);
ANN static void tojson_exp_if(ToJson *a, Exp_If *b);
ANN static void tojson_exp_dot(ToJson *a, Exp_Dot *b);
ANN static void tojson_exp_lambda(ToJson *a, Exp_Lambda *b);
ANN static void tojson_exp_td(ToJson *a, Type_Decl *b);
ANN static void tojson_exp(ToJson *a, Exp b);
ANN static void tojson_stmt_exp(ToJson *a, Stmt_Exp b);
ANN static void tojson_stmt_while(ToJson *a, Stmt_Flow b);
ANN static void tojson_stmt_until(ToJson *a, Stmt_Flow b);
ANN static void tojson_stmt_for(ToJson *a, Stmt_For b);
ANN static void tojson_stmt_each(ToJson *a, Stmt_Each b);
ANN static void tojson_stmt_loop(ToJson *a, Stmt_Loop b);
ANN static void tojson_stmt_if(ToJson *a, Stmt_If b);
ANN static void tojson_stmt_code(ToJson *a, Stmt_Code b);
ANN static void tojson_stmt_varloop(ToJson *a, Stmt_VarLoop b);
ANN static void tojson_stmt_break(ToJson *a, Stmt_Exp b);
ANN static void tojson_stmt_continue(ToJson *a, Stmt_Exp b);
ANN static void tojson_stmt_return(ToJson *a, Stmt_Exp b);
ANN static void tojson_case_list(ToJson *a, Stmt_List b);
ANN static void tojson_stmt_match(ToJson *a, Stmt_Match b);
ANN static void tojson_stmt_case(ToJson *a, Stmt_Match b);
ANN static void tojson_stmt_index(ToJson *a, Stmt_Index b);
ANN static void tojson_stmt_pp(ToJson *a, Stmt_PP b);
ANN static void tojson_stmt_retry(ToJson *a, Stmt_Exp b);
ANN static void tojson_stmt_try(ToJson *a, Stmt_Try b);
ANN static void tojson_stmt_defer(ToJson *a, Stmt_Defer b);
ANN static void tojson_stmt(ToJson *a, Stmt b);
ANN static void tojson_arg_list(ToJson *a, Arg_List b);
ANN static void tojson_union_list(ToJson *a, Union_List b);
ANN static void tojson_stmt_list(ToJson *a, Stmt_List b);
ANN static void tojson_func_base(ToJson *a, Func_Base *b);
ANN static void tojson_func_def(ToJson *a, Func_Def b);
ANN static void tojson_class_def(ToJson *a, Class_Def b);
ANN static void tojson_enum_def(ToJson *a, Enum_Def b);
ANN static void tojson_union_def(ToJson *a, Union_Def b);
ANN static void tojson_fptr_def(ToJson *a, Fptr_Def b);
ANN static void tojson_type_def(ToJson *a, Type_Def b);
ANN static void tojson_extend_def(ToJson *a, Extend_Def b);
ANN static void tojson_section(ToJson *a, Section *b);
ANN static void tojson_ast(ToJson *a, Ast b);

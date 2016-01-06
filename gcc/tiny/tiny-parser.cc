/* Tiny parser
   Copyright (C) 2016 Free Software Foundation, Inc.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <iostream>

#include "tiny/tiny-parser.h"
#include "tiny/tiny-lexer.h"
#include "tiny/tiny-symbol.h"
#include "tiny/tiny-scope.h"
#include "tiny/tiny-context.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"

namespace Tiny
{

namespace
{
// FIXME - Move this to a helper file or, better, see if this already exists
// in GCC
template <typename Accessor> struct tree_chain_base
{
  tree first;
  tree last;

  tree_chain_base () : first (NULL_TREE), last (NULL_TREE) {}

  void
  append (tree t)
  {
    gcc_assert (t != NULL_TREE);
    if (first == NULL)
      {
	first = last = t;
      }
    else
      {
	Accessor () (last) = t;
	last = t;
      }
  }
};

struct tree_chain_accessor
{
  tree &operator() (tree t) { return TREE_CHAIN (t); }
};

struct tree_chain : tree_chain_base<tree_chain_accessor>
{
};

struct block_chain_accessor
{
  tree &operator() (tree t) { return BLOCK_CHAIN (t); }
};

struct block_chain : tree_chain_base<block_chain_accessor>
{
};
}

struct Parser
{
private:
  void skip_after_semicolon ();
  void skip_after_end ();

  bool skip_token (TokenId);
  const Token *expect_token (TokenId);
  void unexpected_token (const Token *);

  // Expression parsing
  int left_binding_power (const Token *tok);
  tree null_denotation (const Token *tok);
  tree left_denotation (const Token *tok, tree left);

  tree parse_expression (int right_binding_power);

  tree coerce_binary_arithmetic (const Token *tok, tree *left, tree *right);
  bool check_logical_operands (const Token *tok, tree left, tree right);

  tree get_printf_addr ();
  tree get_puts_addr ();

  tree get_scanf_addr ();

  tree build_label_decl (const char *name, location_t loc);
  tree build_if_statement (tree bool_expr, tree then_part, tree else_part);
  tree build_while_statement (tree bool_expr, tree while_body);
  tree build_for_statement (Symbol *ind_var, tree lower_bound, tree upper_bound,
			    tree for_body_stmt_list);

  const char *print_type (tree type);

  tree &get_current_stmt_list ();
  tree &get_current_block ();
  tree &get_current_bind_expr ();

  void enter_scope ();
  tree leave_scope (tree &new_block);

  Symbol *query_variable (const std::string &name, location_t loc);
  Symbol *query_integer_variable (const std::string &name, location_t loc);

  void parse_statement_seq (bool (Parser::*done) ());

  bool done_end ();
  bool done_end_or_else ();
  bool done_end_of_file ();

  typedef tree (Parser::*BinaryHandler) (const Token *, tree);
  BinaryHandler get_binary_handler (TokenId id);

#define BINARY_HANDLER_LIST                                                    \
  BINARY_HANDLER (plus, PLUS)                                                  \
  BINARY_HANDLER (minus, MINUS)                                                \
  BINARY_HANDLER (mult, ASTERISK)                                              \
  BINARY_HANDLER (div, SLASH)                                                  \
  BINARY_HANDLER (mod, PERCENT)                                                \
                                                                               \
  BINARY_HANDLER (equal, EQUAL)                                                \
  BINARY_HANDLER (different, DIFFERENT)                                        \
  BINARY_HANDLER (lower_than, LOWER)                                           \
  BINARY_HANDLER (lower_equal, LOWER_OR_EQUAL)                                 \
  BINARY_HANDLER (greater_than, GREATER)                                       \
  BINARY_HANDLER (greater_equal, GREATER_OR_EQUAL)                             \
                                                                               \
  BINARY_HANDLER (logical_and, AND)                                            \
  BINARY_HANDLER (logical_or, OR)

#define BINARY_HANDLER(name, _)                                                \
  tree binary_##name (const Token *tok, tree left);
  BINARY_HANDLER_LIST
#undef BINARY_HANDLER

public:
  Parser (Lexer &lexer_)
    : lexer (lexer_), puts_fn (NULL_TREE), printf_fn (NULL_TREE),
      scanf_fn (NULL_TREE)
  {
  }

  void parse_program ();

  tree parse_statement ();

  tree parse_variable_declaration ();

  tree parse_type ();

  tree parse_assignment_statement ();
  tree parse_if_statement ();
  tree parse_while_statement ();
  tree parse_for_statement ();
  tree parse_read_statement ();
  tree parse_write_statement ();

  tree parse_expression ();
  tree parse_boolean_expression ();
  tree parse_integer_expression ();

private:
  Lexer &lexer;
  Context context;

  tree main_fndecl;

  tree puts_fn;
  tree printf_fn;
  tree scanf_fn;

  std::vector<tree> stack_stmt_list;
  std::vector<tree_chain> stack_var_decl_chain;

  std::vector<block_chain> stack_block_chain;
};

void
Parser::skip_after_semicolon ()
{
  const Token *t = lexer.peek_token ();

  while (t->get_id () != Tiny::END_OF_FILE && t->get_id () != Tiny::SEMICOLON)
    {
      lexer.get_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiny::SEMICOLON)
    lexer.get_token ();
}

void
Parser::skip_after_end ()
{
  const Token *t = lexer.peek_token ();

  while (t->get_id () != Tiny::END_OF_FILE && t->get_id () != Tiny::END)
    {
      lexer.get_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiny::END)
    lexer.get_token ();
}

const Token *
Parser::expect_token (Tiny::TokenId token_id)
{
  const Token *t = lexer.peek_token ();
  if (t->get_id () == token_id)
    {
      lexer.get_token ();
      return t;
    }
  else
    {
      error_at (t->get_locus (), "expecting %s but %s found\n",
		get_token_description (token_id), t->get_token_description ());
      return NULL;
    }
}

bool
Parser::skip_token (Tiny::TokenId token_id)
{
  return expect_token (token_id) != NULL;
}

void
Parser::unexpected_token (const Token *t)
{
  ::error_at (t->get_locus (), "unexpected %s\n",
	      t->get_token_description ());
}

void
Parser::parse_program ()
{
  // Built type of main "int (int, char**)"
  tree main_fndecl_type_param[] = {
    integer_type_node,					     /* int */
    build_pointer_type (build_pointer_type (char_type_node)) /* char** */
  };
  tree main_fndecl_type
    = build_function_type_array (integer_type_node, 2, main_fndecl_type_param);
  // Create function declaration "int main(int, char**)"
  main_fndecl = build_fn_decl ("main", main_fndecl_type);

  // Enter top level scope
  enter_scope ();
  // program -> statement*
  parse_statement_seq (&Parser::done_end_of_file);
  // Append "return 0;"
  tree resdecl
    = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_RESULT (main_fndecl) = resdecl;
  tree set_result
    = build2 (INIT_EXPR, void_type_node, DECL_RESULT (main_fndecl),
	      build_int_cst_type (integer_type_node, 0));
  tree return_stmt = build1 (RETURN_EXPR, void_type_node, set_result);
  append_to_statement_list (return_stmt, &get_current_stmt_list ());

  // Leave top level scope, get its binding expression and its main block
  tree main_block = NULL_TREE;
  tree main_bind_expr = leave_scope (main_block);

  // Finish main function
  BLOCK_SUPERCONTEXT (main_block) = main_fndecl;
  DECL_INITIAL (main_fndecl) = main_block;
  DECL_SAVED_TREE (main_fndecl) = main_bind_expr;

  DECL_EXTERNAL (main_fndecl) = 0;
  DECL_PRESERVE_P (main_fndecl) = 1;

  // Convert from GENERIC to GIMPLE
  gimplify_function_tree (main_fndecl);

  // Insert it into the graph
  cgraph_node::finalize_function (main_fndecl, true);

  main_fndecl = NULL_TREE;
}

bool
Parser::done_end_of_file ()
{
  const Token *t = lexer.peek_token ();
  bool done = false;
  if (t->get_id () == Tiny::END_OF_FILE)
    {
      done = true;
    }

  return done;
}

bool
Parser::done_end ()
{
  const Token *t = lexer.peek_token ();
  bool done = false;
  if (t->get_id () == Tiny::END
          || t->get_id() == Tiny::END_OF_FILE)
    {
      done = true;
    }

  return done;
}

bool
Parser::done_end_or_else ()
{
  const Token *t = lexer.peek_token ();
  bool done = false;
  if (t->get_id () == Tiny::END
      || t->get_id() == Tiny::ELSE
      || t->get_id() == Tiny::END_OF_FILE)
    {
      done = true;
    }

  return done;
}

void
Parser::parse_statement_seq (bool (Parser::*done) ())
{
  // Parse statements until done and append to the current stmt list
  while (!(this->*done) ())
    {
      tree stmt = parse_statement ();
      append_to_statement_list (stmt, &get_current_stmt_list ());
    }
}

void
Parser::enter_scope ()
{
  context.push_scope ();

  tree stmt_list = alloc_stmt_list ();
  stack_stmt_list.push_back (stmt_list);

  stack_var_decl_chain.push_back (tree_chain ());
  stack_block_chain.push_back (block_chain ());
}

tree
Parser::leave_scope (tree &new_block)
{
  tree current_stmt_list = get_current_stmt_list ();
  stack_stmt_list.pop_back ();

  tree_chain var_decl_chain = stack_var_decl_chain.back ();
  stack_var_decl_chain.pop_back ();

  block_chain subblocks = stack_block_chain.back ();
  stack_block_chain.pop_back ();

  new_block = build_block (var_decl_chain.first, subblocks.first,
			   /* supercontext */ NULL_TREE, /* chain */ NULL_TREE);

  // Add the new block to the current chain of blocks (if any)
  if (!stack_block_chain.empty ())
    {
      stack_block_chain.back ().append (new_block);
    }

  // Set the subblocks to have the new block as their parent
  for (tree it = subblocks.first; it != NULL_TREE; it = BLOCK_CHAIN (it))
    BLOCK_SUPERCONTEXT (it) = new_block;

  tree bind_expr = build3 (BIND_EXPR, void_type_node, NULL_TREE,
			   current_stmt_list, new_block);
  BIND_EXPR_VARS (bind_expr) = var_decl_chain.first;

  return bind_expr;
}

tree &
Parser::get_current_stmt_list ()
{
  return stack_stmt_list.back ();
}

tree
Parser::parse_statement ()
{
  /*
    statement ->  variable_declaration
	   |  assignment_statement
	   |  if_statement
	   |  while_statement
	   |  for_statement
	   |  read_statement
	   |  write_statement
	   */
  const Token *t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case Tiny::VAR:
      return parse_variable_declaration ();
      break;
    case Tiny::IF:
      return parse_if_statement ();
      break;
    case Tiny::WHILE:
      return parse_while_statement ();
      break;
    case Tiny::FOR:
      return parse_for_statement ();
      break;
    case Tiny::READ:
      return parse_read_statement ();
      break;
    case Tiny::WRITE:
      return parse_write_statement ();
      break;
    case Tiny::IDENTIFIER:
      return parse_assignment_statement ();
    default:
      unexpected_token (t);
      skip_after_semicolon ();
      return error_mark_node;
    }
}

tree
Parser::parse_variable_declaration ()
{
  // variable_declaration -> "var" identifier ":" type ";"
  if (!skip_token (Tiny::VAR))
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  const Token *identifier = expect_token (Tiny::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  if (!skip_token (Tiny::COLON))
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  tree type_tree = parse_type ();

  skip_token (Tiny::SEMICOLON);

  if (error_operand_p (type_tree))
    return error_mark_node;

  if (context.scope ().query_in_scope (identifier->get_str ()))
    {
      error_at (identifier->get_locus (),
		"variable '%s' already declared in this scope",
		identifier->get_str ().c_str ());
    }
  Symbol *sym = new Symbol (identifier->get_str ());
  context.scope ().insert (sym);

  tree decl = build_decl (identifier->get_locus (), VAR_DECL, 
			    get_identifier (sym->get_global_name ().c_str ()),
                            type_tree);

  gcc_assert(!stack_var_decl_chain.empty());
  stack_var_decl_chain.back().append(decl);

  sym->set_tree_decl (decl);

  tree stmt
    = ::build1_loc (identifier->get_locus (), DECL_EXPR, void_type_node, decl);

  return stmt;
}

namespace
{

bool
is_string_type (tree type)
{
  gcc_assert (TYPE_P (type));
  return TREE_CODE (type) == POINTER_TYPE
	 && TYPE_MAIN_VARIANT (TREE_TYPE (type)) == char_type_node;
}
}

const char *
Parser::print_type (tree type)
{
  gcc_assert (TYPE_P (type));

  if (type == void_type_node)
    {
      return "void";
    }
  else if (type == integer_type_node)
    {
      return "int";
    }
  else if (type == float_type_node)
    {
      return "float";
    }
  else if (is_string_type (type))
    {
      return "string";
    }
  else if (type == boolean_type_node)
    {
      return "boolean";
    }
  else
    {
      return "<<unknown-type>>";
    }
}

tree
Parser::parse_type ()
{
  // type -> "int"
  //      | "float"

  const Token *t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case Tiny::INT:
      lexer.get_token ();
      return ::integer_type_node;
      break;
    case Tiny::FLOAT:
      lexer.get_token ();
      return ::float_type_node;
      break;
    default:
      unexpected_token (t);
      skip_after_semicolon ();
      return ::error_mark_node;
      break;
    }
}

Symbol* Parser::query_variable(const std::string& name, location_t loc)
{
  Symbol *sym = context.scope ().query (name);
  if (sym == NULL)
    {
      error_at (loc,
		"variable '%s' not declared in the current scope",
		name.c_str());
    }
  return sym;
}

Symbol *
Parser::query_integer_variable (const std::string &name, location_t loc)
{
  Symbol *sym = query_variable (name, loc);
  if (sym != NULL)
    {
      tree var_decl = sym->get_tree_decl ();
      gcc_assert (var_decl != NULL_TREE);

      if (TREE_TYPE (var_decl) != integer_type_node)
	{
	  error_at (loc, "variable '%s' does not have integer type",
		    name.c_str ());
	  sym = NULL;
	}
    }

  return sym;
}

tree
Parser::parse_assignment_statement ()
{
  // assignment_statement -> identifier ":=" expression ";"
  const Token *identifier = expect_token(Tiny::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon();
      return error_mark_node;
    }

  Symbol *sym = query_variable(identifier->get_str(), identifier->get_locus());
  if (sym == NULL)
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  gcc_assert(sym->get_tree_decl() != NULL_TREE);
  tree var_decl = sym->get_tree_decl();

  const Token* assig_tok = expect_token(Tiny::ASSIG);
  if (assig_tok == NULL)
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  const Token *first_of_expr = lexer.peek_token();

  tree expr = parse_expression ();
  if (error_operand_p(expr))
    return error_mark_node;

  skip_token (Tiny::SEMICOLON);

  // FIXME: We may want to allow coercions here
  if (TREE_TYPE(var_decl) != TREE_TYPE(expr))
    {
      error_at(first_of_expr->get_locus(),
               "cannot assign value of type %s to variable '%s' of type %s",
               print_type(TREE_TYPE(expr)),
               sym->get_name().c_str(),
               print_type(TREE_TYPE(var_decl)));
      return error_mark_node;
    }

  tree assig_expr = build2_loc(assig_tok->get_locus(), MODIFY_EXPR, void_type_node, var_decl, expr);

  return assig_expr;
} 

tree Parser::build_label_decl(const char* name, location_t loc)
{
  tree t = build_decl(loc, LABEL_DECL, get_identifier(name), void_type_node);

  gcc_assert(main_fndecl != NULL_TREE);
  DECL_CONTEXT(t) = main_fndecl;
  
  return t;
}

tree
Parser::build_if_statement (tree bool_expr, tree then_part, tree else_part)
{
  if (error_operand_p (bool_expr))
    return error_mark_node;

  tree then_label_decl = build_label_decl ("then", EXPR_LOCATION (then_part));

  tree else_label_decl = NULL_TREE;
  if (else_part != NULL_TREE)
    else_label_decl = build_label_decl ("else", EXPR_LOCATION (else_part));

  tree endif_label_decl
    = build_label_decl ("end_if", EXPR_LOCATION (then_part));

  tree goto_then = build1_loc (EXPR_LOCATION (bool_expr), GOTO_EXPR,
			       void_type_node, then_label_decl);
  tree goto_endif = build1_loc (EXPR_LOCATION (bool_expr), GOTO_EXPR,
				void_type_node, endif_label_decl);

  tree goto_else_or_endif;
  if (else_part != NULL_TREE)
    goto_else_or_endif = build1_loc (EXPR_LOCATION (bool_expr), GOTO_EXPR,
				     void_type_node, else_label_decl);
  else
    goto_else_or_endif = goto_endif;

  tree stmt_list = alloc_stmt_list ();

  tree cond_expr
    = build3_loc (EXPR_LOCATION (bool_expr), COND_EXPR, void_type_node,
		  bool_expr, goto_then, goto_else_or_endif);
  append_to_statement_list (cond_expr, &stmt_list);

  tree then_label_expr = build1_loc (EXPR_LOCATION (then_part), LABEL_EXPR,
				     void_type_node, then_label_decl);
  append_to_statement_list (then_label_expr, &stmt_list);

  append_to_statement_list (then_part, &stmt_list);

  if (else_part != NULL_TREE)
    {
      // Make sure after then part has been executed we go to the end if
      append_to_statement_list (goto_endif, &stmt_list);

      tree else_label_expr = build1_loc (EXPR_LOCATION (else_part), LABEL_EXPR,
					 void_type_node, else_label_decl);
      append_to_statement_list (else_label_expr, &stmt_list);

      append_to_statement_list (else_part, &stmt_list);
    }

  // FIXME - location
  tree endif_label_expr = build1_loc (UNKNOWN_LOCATION, LABEL_EXPR,
				      void_type_node, endif_label_decl);
  append_to_statement_list (endif_label_expr, &stmt_list);

  return stmt_list;
}

tree
Parser::parse_if_statement ()
{
  if (!skip_token (Tiny::IF))
    {
      skip_after_end ();
      return error_mark_node;
    }

  tree expr = parse_boolean_expression ();

  skip_token (Tiny::THEN);

  enter_scope ();
  parse_statement_seq (&Parser::done_end_or_else);
  tree then_block = NULL_TREE;
  tree then_stmt_list = leave_scope (then_block);

  tree else_block = NULL_TREE;
  tree else_stmt_list = NULL_TREE;
  const Token *tok = lexer.peek_token ();
  if (tok->get_id () == Tiny::ELSE)
    {
      // Consume 'else'
      skip_token (Tiny::ELSE);

      enter_scope ();
      parse_statement_seq (&Parser::done_end);
      else_stmt_list = leave_scope (else_block);

      // Consume 'end'
      skip_token (Tiny::END);
    }
  else if (tok->get_id () == Tiny::END)
    {
      // Consume 'end'
      skip_token (Tiny::END);
    }
  else
    {
      unexpected_token (tok);
      return error_mark_node;
    }

  return build_if_statement (expr, then_stmt_list, else_stmt_list);
}

tree
Parser::build_while_statement (tree bool_expr, tree while_body)
{
  if (error_operand_p (bool_expr))
    return error_mark_node;

  tree stmt_list = alloc_stmt_list ();

  tree while_check_label_decl
    = build_label_decl ("while_check", EXPR_LOCATION (bool_expr));

  tree while_check_label_expr
    = build1_loc (EXPR_LOCATION (bool_expr), LABEL_EXPR, void_type_node,
		  while_check_label_decl);
  append_to_statement_list (while_check_label_expr, &stmt_list);

  tree while_body_label_decl
    = build_label_decl ("while_body", EXPR_LOCATION (while_body));
  tree end_of_while_label_decl
    // FIXME - location
    = build_label_decl ("end_of_while", UNKNOWN_LOCATION);

  tree cond_expr
    = build3_loc (EXPR_LOCATION (bool_expr), COND_EXPR, void_type_node,
		  bool_expr, build1_loc (EXPR_LOCATION (bool_expr), GOTO_EXPR,
					 void_type_node, while_body_label_decl),
		  build1_loc (EXPR_LOCATION (bool_expr), GOTO_EXPR,
			      void_type_node, end_of_while_label_decl));
  append_to_statement_list (cond_expr, &stmt_list);

  tree while_body_label_expr
    = build1_loc (EXPR_LOCATION (while_body), LABEL_EXPR, void_type_node,
		  while_body_label_decl);
  append_to_statement_list (while_body_label_expr, &stmt_list);

  append_to_statement_list (while_body, &stmt_list);

  // FIXME - location
  tree goto_check = build1_loc (UNKNOWN_LOCATION, GOTO_EXPR, void_type_node,
				while_check_label_decl);
  append_to_statement_list (goto_check, &stmt_list);

  // FIXME - location
  tree end_of_while_label_expr
    = build1_loc (UNKNOWN_LOCATION, LABEL_EXPR, void_type_node,
		  end_of_while_label_decl);
  append_to_statement_list (end_of_while_label_expr, &stmt_list);

  return stmt_list;
}

tree
Parser::parse_while_statement ()
{
  if (!skip_token (Tiny::WHILE))
    {
      skip_after_end ();
      return error_mark_node;
    }

  tree expr = parse_boolean_expression ();
  if (!skip_token (Tiny::DO))
    {
      skip_after_end ();
      return error_mark_node;
    }

  enter_scope ();
  parse_statement_seq (&Parser::done_end);
  tree while_body_block = NULL_TREE;
  tree while_body_list_stmt = leave_scope (while_body_block);

  skip_token (Tiny::END);

  return build_while_statement (expr, while_body_list_stmt);
}

tree
Parser::build_for_statement (Symbol *ind_var, tree lower_bound,
			     tree upper_bound, tree for_body_stmt_list)
{
  if (ind_var == NULL)
    return error_mark_node;
  tree ind_var_decl = ind_var->get_tree_decl ();

  // Lower
  if (error_operand_p (lower_bound))
    return error_mark_node;

  // Upper
  if (error_operand_p (upper_bound))
    return error_mark_node;

  // ind_var := lower;
  tree stmt_list = alloc_stmt_list ();

  tree init_ind_var
    = build2_loc (/* FIXME */ UNKNOWN_LOCATION, MODIFY_EXPR, void_type_node,
		  ind_var_decl, lower_bound);
  append_to_statement_list (init_ind_var, &stmt_list);

  // ind_var <= upper
  tree while_condition
    = build2_loc (EXPR_LOCATION (upper_bound), LE_EXPR, boolean_type_node,
		  ind_var_decl, upper_bound);

  // for-body
  // ind_var := ind_var + 1
  tree incr_ind_var
    = build2_loc (/* FIXME */ UNKNOWN_LOCATION, MODIFY_EXPR, void_type_node,
		  ind_var_decl,
		  build2_loc (UNKNOWN_LOCATION, PLUS_EXPR, integer_type_node,
			      ind_var_decl,
			      build_int_cst_type (::integer_type_node, 1)));
  append_to_statement_list (incr_ind_var, &for_body_stmt_list);

  // construct the associated while statement
  tree while_stmt = build_while_statement (while_condition, for_body_stmt_list);
  append_to_statement_list (while_stmt, &stmt_list);

  return stmt_list;
}

tree
Parser::parse_for_statement ()
{
  if (!skip_token (Tiny::FOR))
    {
      skip_after_end ();
      return error_mark_node;
    }

  const Token *identifier = expect_token (Tiny::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_end ();
      return error_mark_node;
    }

  if (!skip_token (Tiny::ASSIG))
    {
      skip_after_end ();
      return error_mark_node;
    }

  tree lower_bound = parse_integer_expression ();

  if (!skip_token (Tiny::TO))
    {
      skip_after_end ();
      return error_mark_node;
    }

  tree upper_bound = parse_integer_expression ();

  if (!skip_token (Tiny::DO))
    {
      skip_after_end ();
      return error_mark_node;
    }

  enter_scope ();
  parse_statement_seq (&Parser::done_end);

  tree for_body_block = NULL_TREE;
  tree for_body_stmt_list = leave_scope (for_body_block);

  skip_token (Tiny::END);

  // Induction var
  Symbol *ind_var
    = query_integer_variable (identifier->get_str (), identifier->get_locus ());

  return build_for_statement(ind_var, lower_bound, upper_bound, for_body_stmt_list);
}

tree
Parser::get_scanf_addr()
{
  if (scanf_fn == NULL_TREE)
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      scanf_fn = build_fn_decl ("scanf", fndecl_type);
      DECL_EXTERNAL (scanf_fn) = 1;

      scanf_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), scanf_fn);
    }

  return scanf_fn;
}

tree
Parser::parse_read_statement ()
{
  if (!skip_token (Tiny::READ))
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  const Token *first_of_expr = lexer.peek_token ();
  tree expr = parse_expression ();

  skip_token (Tiny::SEMICOLON);

  if (error_operand_p (expr))
    return error_mark_node;

  if (TREE_CODE (expr) != VAR_DECL)
    {
      error_at (first_of_expr->get_locus (),
		"invalid expression in read statement");
      return error_mark_node;
    }

  // Now this variable must be addressable
  TREE_ADDRESSABLE (expr) = 1;

  const char* format = NULL;
  if (TREE_TYPE (expr) == integer_type_node)
    {
      format = "%d";
    }
  else if (TREE_TYPE (expr) == float_type_node)
    {
      format = "%f";
    }
  else
    {
      error_at (first_of_expr->get_locus (),
		"variable of type %s is not a valid read operand",
		print_type (TREE_TYPE (expr)));
      return error_mark_node;
    }

  tree args[]
    = {build_string_literal (strlen (format) + 1, format),
        build1_loc (first_of_expr->get_locus (), ADDR_EXPR,
                    build_pointer_type (TREE_TYPE(expr)), expr)};

  tree scanf_fn = get_scanf_addr ();

  tree stmt = build_call_array_loc (first_of_expr->get_locus (),
                                    integer_type_node, scanf_fn, 2, args);

  return stmt;
}

tree
Parser::get_puts_addr ()
{
  if (puts_fn == NULL_TREE)
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (integer_type_node, 1, fndecl_type_param);

      puts_fn = build_fn_decl ("puts", fndecl_type);
      DECL_EXTERNAL (puts_fn) = 1;

      puts_fn = build1 (ADDR_EXPR, build_pointer_type (fndecl_type), puts_fn);
    }

  return puts_fn;
}

tree
Parser::get_printf_addr ()
{
  if (printf_fn == NULL_TREE)
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      printf_fn = build_fn_decl ("printf", fndecl_type);
      DECL_EXTERNAL (printf_fn) = 1;

      printf_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), printf_fn);
    }

  return printf_fn;
}

tree
Parser::parse_write_statement ()
{
  // write_statement -> "write" expression ";"

  if (!skip_token (Tiny::WRITE))
    {
      skip_after_semicolon ();
      return error_mark_node;
    }

  const Token *first_of_expr = lexer.peek_token ();
  tree expr = parse_expression ();

  skip_token (Tiny::SEMICOLON);

  if (error_operand_p (expr))
    return error_mark_node;

  tree stmt = error_mark_node;

  if (TREE_TYPE (expr) == integer_type_node)
    {
      // printf("%d\n", expr)
      const char *format_integer = "%d\n";
      tree args[]
	= {build_string_literal (strlen (format_integer) + 1, format_integer),
	   expr};

      tree printf_fn = get_printf_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (),
					integer_type_node, printf_fn, 2, args);

      return stmt;
    }
  else if (TREE_TYPE (expr) == float_type_node)
    {
      // printf("%f\n", (double)expr)
      const char *format_float = "%f\n";
      tree args[]
	= {build_string_literal (strlen (format_float) + 1, format_float),
	   convert (double_type_node, expr)};

      tree printf_fn = get_printf_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (),
					integer_type_node, printf_fn, 2, args);

      return stmt;
    }
  else if (is_string_type (TREE_TYPE (expr)))
    {
      // Alternatively we could use printf('%s\n', expr) instead of puts(expr)
      tree args[] = {expr};

      tree puts_fn = get_puts_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (),
					integer_type_node, puts_fn, 1, args);
      return stmt;
    }
  else
    {
      error_at (first_of_expr->get_locus (),
		"value of type %s is not a valid write operand",
		print_type (TREE_TYPE (expr)));
    }

  return stmt;
}

// This is a Pratt parser
tree
Parser::parse_expression (int right_binding_power)
{
  const Token *current_token = lexer.peek_token ();
  lexer.get_token (); // This is silly but it matches the code below
  const Token *next_token = lexer.peek_token ();

  tree expr = null_denotation (current_token);

  if (error_operand_p (expr))
    return error_mark_node;

  while (right_binding_power < left_binding_power (next_token))
    {
      current_token = next_token;
      lexer.get_token ();
      next_token = lexer.peek_token ();

      expr = left_denotation (current_token, expr);
      if (error_operand_p (expr))
	return error_mark_node;
    }

  return expr;
}

namespace
{
enum binding_powers
{
  // Highest priority
  LBP_HIGHEST = 100,

  LBP_UNARY_PLUS = 50,  // Used only when the null denotation is +
  LBP_UNARY_MINUS = 50, // Used only when the null denotation is -

  LBP_MUL = 40,
  LBP_DIV = LBP_MUL,
  LBP_MOD = LBP_MUL,

  LBP_PLUS = 30,
  LBP_MINUS = LBP_PLUS,

  LBP_EQUAL = 20,
  LBP_DIFFERENT = LBP_EQUAL,
  LBP_LOWER_THAN = LBP_EQUAL,
  LBP_LOWER_EQUAL = LBP_EQUAL,
  LBP_GREATER_THAN = LBP_EQUAL,
  LBP_GREATER_EQUAL = LBP_EQUAL,

  LBP_LOGICAL_AND = 10,
  LBP_LOGICAL_OR = 10,
  LBP_LOGICAL_NOT = 10,

    // Lowest priority
  LBP_LOWEST
  = 0,
};
}

// This implements priorities
int
Parser::left_binding_power (const Token *token)
{
  switch (token->get_id ())
    {
    //
    case Tiny::ASTERISK:
      return LBP_MUL;
    case Tiny::SLASH:
      return LBP_DIV;
    case Tiny::PERCENT:
      return LBP_MOD;
    //
    case Tiny::PLUS:
      return LBP_PLUS;
    case Tiny::MINUS:
      return LBP_MINUS;
    //
    case Tiny::EQUAL:
      return LBP_EQUAL;
    case Tiny::DIFFERENT:
      return LBP_DIFFERENT;
    case Tiny::GREATER:
      return LBP_GREATER_THAN;
    case Tiny::GREATER_OR_EQUAL:
      return LBP_GREATER_EQUAL;
    case Tiny::LOWER:
      return LBP_LOWER_THAN;
    case Tiny::LOWER_OR_EQUAL:
      return LBP_LOWER_EQUAL;
    // Anything that cannot appear as a left operand
    // is considered a terminator
    default:
      return LBP_LOWEST;
    }
}

// This is invoked when a token (including prefix operands) is found at a
// "prefix" position
tree
Parser::null_denotation (const Token *tok)
{
  switch (tok->get_id ())
    {
    case Tiny::IDENTIFIER:
      {
	Symbol *s = context.scope ().query (tok->get_str ());
	if (s == NULL)
	  {
	    error_at (tok->get_locus (),
		      "variable '%s' not declared in the current scope",
		      tok->get_str ().c_str ());
	    return error_mark_node;
	  }
	return s->get_tree_decl ();
      }
    case Tiny::INTEGER_LITERAL:
      // FIXME : check ranges
      return build_int_cst_type (::integer_type_node,
				 atoi (tok->get_str ().c_str ()));
      break;
    case Tiny::REAL_LITERAL:
      {
	REAL_VALUE_TYPE real_value;
	real_from_string3 (&real_value, tok->get_str ().c_str (),
			   SFmode /* SF = float */);

	return build_real (float_type_node, real_value);
      }
      break;
    case Tiny::STRING_LITERAL:
      {
	std::string str = tok->get_str ();
	const char *c_str = str.c_str ();
	return build_string_literal (::strlen (c_str) + 1, c_str);
      }
      break;
    case Tiny::LEFT_PAREN:
      {
	tree expr = parse_expression ();
	tok = lexer.peek_token ();
	if (tok->get_id () != Tiny::RIGHT_PAREN)
	  error_at (tok->get_locus (), "expecting ')' but %s found\n",
		    tok->get_token_description ());
	else
	  lexer.get_token ();
	return expr;
      }
    case Tiny::PLUS:
      {
	tree expr = parse_expression (LBP_UNARY_PLUS);
	if (error_operand_p (expr))
	  return expr;
	if (TREE_TYPE (expr) != integer_type_node
	    || TREE_TYPE (expr) != float_type_node)
	  {
	    error_at (tok->get_locus (),
		      "operand of unary plus must be int or float but it is %s",
		      print_type (TREE_TYPE (expr)));
	    return error_mark_node;
	  }
	return expr;
      }
    case Tiny::MINUS:
      {
	tree expr = parse_expression (LBP_UNARY_MINUS);
	if (error_operand_p (expr))
	  return expr;
	if (TREE_TYPE (expr) != integer_type_node
	    || TREE_TYPE (expr) != float_type_node)
	  {
	    error_at (
	      tok->get_locus (),
	      "operand of unary minus must be int or float but it is %s",
	      print_type (TREE_TYPE (expr)));
	    return error_mark_node;
	  }
	expr
	  = build1_loc (tok->get_locus (), NEGATE_EXPR, TREE_TYPE (expr), expr);
	return expr;
      }
    case Tiny::NOT:
      {
	tree expr = parse_expression (LBP_LOGICAL_NOT);
	if (error_operand_p (expr))
	  return expr;

	if (TREE_TYPE (expr) != boolean_type_node)
	  {
	    error_at (tok->get_locus (),
		      "operand of logical not must be a boolean but it is %s",
		      print_type (TREE_TYPE (expr)));
	    return error_mark_node;
	  }

	expr
	  = build1_loc (tok->get_locus (), TRUTH_NOT_EXPR, boolean_type_node, expr);
	return expr;
      }
    default:
      unexpected_token (tok);
      return error_mark_node;
    }
}

tree
Parser::coerce_binary_arithmetic (const Token* tok, tree *left, tree *right)
{
  tree left_type = TREE_TYPE (*left);
  tree right_type = TREE_TYPE (*right);

  if (error_operand_p (left_type) || error_operand_p (right_type))
    {
      return error_mark_node;
    }

  if (left_type == right_type)
    {
      if (left_type == integer_type_node || left_type == float_type_node)
	{
	  return left_type;
	}
    }
  else if ((left_type == integer_type_node && right_type == float_type_node)
	   || (left_type == float_type_node && right_type == integer_type_node))
    {
      // We will coerce the integer into a float
      if (left_type == integer_type_node)
	{
	  *left = build1 (CONVERT_EXPR, float_type_node, *left);
	}
      else
	{
	  *right = build1 (CONVERT_EXPR, float_type_node, *right);
	}
      return float_type_node;
    }

  // i.e. int + boolean
  error_at (tok->get_locus (), "invalid operands of type %s and %s for operator %s",
	    print_type (left_type), print_type (right_type), tok->get_token_description());
  return error_mark_node;
}

Parser::BinaryHandler
Parser::get_binary_handler (TokenId id)
{
  switch (id)
    {
#define BINARY_HANDLER(name, token_id)                                         \
  case Tiny::token_id:                                                         \
    return &Parser::binary_##name;
      BINARY_HANDLER_LIST
#undef BINARY_HANDLER
    default:
      return NULL;
    }
}

tree
Parser::binary_plus (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_PLUS);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), PLUS_EXPR, tree_type, left, right);
}

tree
Parser::binary_minus (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_MINUS);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), MINUS_EXPR, tree_type, left, right);
}

tree
Parser::binary_mult (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_MUL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), MULT_EXPR, tree_type, left, right);
}

tree
Parser::binary_div (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_DIV);
  if (error_operand_p (right))
    return error_mark_node;

  if (TREE_TYPE (left) == integer_type_node
      && TREE_TYPE (right) == integer_type_node)
    {
      // Integer division (truncating, like in C)
      return build2_loc (tok->get_locus (), TRUNC_DIV_EXPR, integer_type_node,
			 left, right);
    }
  else
    {
      // Real division
      tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
      if (error_operand_p(tree_type))
        return error_mark_node;

      gcc_assert (tree_type == float_type_node);

      return build2_loc (tok->get_locus (), RDIV_EXPR, tree_type, left, right);
    }
}

tree
Parser::binary_mod (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_MOD);
  if (error_operand_p (right))
    return error_mark_node;

  if (TREE_TYPE (left) == integer_type_node
      && TREE_TYPE (right) == integer_type_node)
    {
      // Integer division (truncating, like in C)
      return build2_loc (tok->get_locus (), TRUNC_MOD_EXPR, integer_type_node,
			 left, right);
    }
  else
    {
      error_at (tok->get_locus (),
		"operands of modulus must be of integer type");
      return error_mark_node;
    }
}

tree
Parser::binary_equal (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), EQ_EXPR, boolean_type_node, left,
		     right);
}

tree
Parser::binary_different (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), NE_EXPR, boolean_type_node, left,
		     right);
}

tree
Parser::binary_lower_than (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), LT_EXPR, boolean_type_node, left,
		     right);
}

tree
Parser::binary_lower_equal (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), LE_EXPR, boolean_type_node, left,
		     right);
}

tree
Parser::binary_greater_than (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), GT_EXPR, boolean_type_node, left,
		     right);
}

tree
Parser::binary_greater_equal (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (error_operand_p(tree_type))
    return error_mark_node;

  return build2_loc (tok->get_locus (), GE_EXPR, boolean_type_node, left,
		     right);
}

bool Parser::check_logical_operands(const Token* tok, tree left, tree right)
{
  if (TREE_TYPE(left) != boolean_type_node
      || TREE_TYPE(right) != boolean_type_node)
    {
      error_at(tok->get_locus(), "operands of operator %s must be boolean but they are %s and %s\n",
               tok->get_token_description(),
               print_type(TREE_TYPE(left)),
               print_type(TREE_TYPE(right)));
      return false;
    }

  return true;
}

tree
Parser::binary_logical_and (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  if (!check_logical_operands(tok, left, right))
    return error_mark_node;

  return build2_loc (tok->get_locus (), TRUTH_ANDIF_EXPR, boolean_type_node, left,
                     right);
}

tree
Parser::binary_logical_or (const Token *tok, tree left)
{
  tree right = parse_expression (LBP_EQUAL);
  if (error_operand_p (right))
    return error_mark_node;

  if (!check_logical_operands(tok, left, right))
    return error_mark_node;

  return build2_loc (tok->get_locus (), TRUTH_ORIF_EXPR, boolean_type_node, left,
                     right);
}

// This is invoked when a token (likely an operand) is found at a (likely
// infix) non-prefix position
tree
Parser::left_denotation (const Token *tok, tree left)
{
  BinaryHandler binary_handler = get_binary_handler(tok->get_id());
  if (binary_handler == NULL)
    {
      unexpected_token (tok);
      return error_mark_node;
    }

  return (this->*binary_handler)(tok, left);
}

tree
Parser::parse_expression ()
{
  return parse_expression (/* right_binding_power */ 0);
}

tree
Parser::parse_boolean_expression ()
{
  tree expr = parse_expression();
  if (error_operand_p(expr))
    return expr;

  if (TREE_TYPE(expr) != boolean_type_node)
    {
      error_at(EXPR_LOCATION(expr), "expected expression of boolean type but its type is %s",
               print_type(TREE_TYPE(expr)));
      return error_mark_node;
    }
  return expr;
}

tree
Parser::parse_integer_expression ()
{
  tree expr = parse_expression();
  if (error_operand_p(expr))
    return expr;

  if (TREE_TYPE(expr) != integer_type_node)
    {
      error_at(EXPR_LOCATION(expr), "expected expression of integer type but its type is %s",
               print_type(TREE_TYPE(expr)));
      return error_mark_node;
    }
  return expr;
}

}

// ------------------------------------------------------
// ------------------------------------------------------
// ------------------------------------------------------

static void tiny_parse_file (const char *filename);

void
tiny_parse_files (int num_files, const char **files)
{
  for (int i = 0; i < num_files; i++)
    {
      tiny_parse_file (files[i]);
    }
}

static void
tiny_parse_file (const char *filename)
{
  // FIXME: handle stdin "-"
  FILE *file = fopen (filename, "r");
  if (file == NULL)
    {
      fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

  Tiny::Lexer lexer (filename, file);
  Tiny::Parser parser (lexer);

  parser.parse_program ();

  fclose (file);
}

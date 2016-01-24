#ifndef TINY_TOKEN_H
#define TINY_TOKEN_H

#include "config.h"
#include "system.h"
#include "input.h"

#include <string>
#include <tr1/memory>

namespace Tiny
{

// TINY_TOKEN(name, description)
// TINY_TOKEN_KEYWORD(name, identifier)
//
// Keep TINY_TOKEN_KEYWORD sorted

#define TINY_TOKEN_LIST                                                        \
  TINY_TOKEN (FIRST_TOKEN, "<first-token-marker>")                             \
  TINY_TOKEN (END_OF_FILE, "end of file")                                      \
  TINY_TOKEN (ASSIG, ":=")                                                     \
  TINY_TOKEN (ASTERISK, "*")                                                   \
  TINY_TOKEN (COLON, ":")                                                      \
  TINY_TOKEN (COMMA, ",")                                                      \
  TINY_TOKEN (DIFFERENT, "!=")                                                 \
  TINY_TOKEN (EQUAL, "=")                                                      \
  TINY_TOKEN (LEFT_PAREN, "(")                                                 \
  TINY_TOKEN (MINUS, "-")                                                      \
  TINY_TOKEN (PLUS, "+")                                                       \
  TINY_TOKEN (RIGHT_PAREN, ")")                                                \
  TINY_TOKEN (SEMICOLON, ";")                                                  \
  TINY_TOKEN (SLASH, "/")                                                      \
  TINY_TOKEN (PERCENT, "%")                                                    \
  TINY_TOKEN (GREATER, ">")                                                    \
  TINY_TOKEN (GREATER_OR_EQUAL, ">=")                                          \
  TINY_TOKEN (LOWER, "<")                                                      \
  TINY_TOKEN (LOWER_OR_EQUAL, "<=")                                            \
  TINY_TOKEN (IDENTIFIER, "identifier")                                        \
  TINY_TOKEN (INTEGER_LITERAL, "integer literal")                              \
  TINY_TOKEN (REAL_LITERAL, "real literal")                                    \
  TINY_TOKEN (STRING_LITERAL, "string literal")                                \
  TINY_TOKEN (LEFT_SQUARE, "[")                                                \
  TINY_TOKEN (RIGHT_SQUARE, "]")                                               \
                                                                               \
  TINY_TOKEN_KEYWORD (AND, "and")                                              \
  TINY_TOKEN_KEYWORD (DO, "do")                                                \
  TINY_TOKEN_KEYWORD (ELSE, "else")                                            \
  TINY_TOKEN_KEYWORD (END, "end")                                              \
  TINY_TOKEN_KEYWORD (FLOAT, "float")                                          \
  TINY_TOKEN_KEYWORD (FOR, "for")                                              \
  TINY_TOKEN_KEYWORD (IF, "if")                                                \
  TINY_TOKEN_KEYWORD (INT, "int")                                              \
  TINY_TOKEN_KEYWORD (NOT, "not")                                              \
  TINY_TOKEN_KEYWORD (OR, "or")                                                \
  TINY_TOKEN_KEYWORD (READ, "read")                                            \
  TINY_TOKEN_KEYWORD (THEN, "then")                                            \
  TINY_TOKEN_KEYWORD (TO, "to")                                                \
  TINY_TOKEN_KEYWORD (VAR, "var")                                              \
  TINY_TOKEN_KEYWORD (WHILE, "while")                                          \
  TINY_TOKEN_KEYWORD (WRITE, "write")                                          \
                                                                               \
  TINY_TOKEN (LAST_TOKEN, "<last-token-marker>")

enum /* class */ TokenId
{
#define TINY_TOKEN(name, _) name,
#define TINY_TOKEN_KEYWORD(x, y) TINY_TOKEN (x, y)
  TINY_TOKEN_LIST
#undef TINY_TOKEN_KEYWORD
#undef TINY_TOKEN
};

const char *get_token_description (TokenId tid);
const char *token_id_to_str (TokenId tid);

struct Token;
typedef std::tr1::shared_ptr<Token> TokenPtr;
typedef std::tr1::shared_ptr<const Token> const_TokenPtr;

struct Token
{
private:
  TokenId token_id;
  location_t locus;
  std::string *str;

  Token (TokenId token_id_, location_t locus_)
    : token_id (token_id_), locus (locus_), str (0)
  {
  }
  Token (TokenId token_id_, location_t locus_, const std::string& str_)
    : token_id (token_id_), locus (locus_), str (new std::string (str_))
  {
  }

  // No default initializer
  Token ();
  // Do not copy/assign tokens
  Token (const Token &);
  Token &operator=(const Token &);

public:
  ~Token () { delete str; }

  static TokenPtr
  make (TokenId token_id, location_t locus)
  {
    return TokenPtr(new Token (token_id, locus));
  }

  static TokenPtr
  make_identifier (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (IDENTIFIER, locus, str));
  }

  static TokenPtr
  make_integer (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (INTEGER_LITERAL, locus, str));
  }

  static TokenPtr
  make_real (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (REAL_LITERAL, locus, str));
  }

  static TokenPtr
  make_string (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (STRING_LITERAL, locus, str));
  }

  TokenId
  get_id () const
  {
    return token_id;
  }

  location_t
  get_locus () const
  {
    return locus;
  }

  const std::string &
  get_str () const
  {
    gcc_assert (str != NULL);
    return *str;
  }

  // diagnostics
  const char *
  get_token_description () const
  {
    return Tiny::get_token_description (token_id);
  }

  // debugging
  const char *
  token_id_to_str () const
  {
    return Tiny::token_id_to_str (token_id);
  }
};

}

#endif // TINY_TOKEN_H

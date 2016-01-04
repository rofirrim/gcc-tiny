#ifndef TINY_LEXER_H
#define TINY_LEXER_H

#include "config.h"
#include "system.h"
#include "input.h"

#include "tiny-buffered-queue.h"

#include <string>
#include <vector>

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
  Token (TokenId token_id_, location_t locus_, const char *str_)
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

  static Token *
  make (TokenId token_id, location_t locus)
  {
    return new Token (token_id, locus);
  }

  static Token *
  make_identifier (location_t locus, const char *str)
  {
    return new Token (IDENTIFIER, locus, str);
  }

  static Token *
  make_integer (location_t locus, const char *str)
  {
    return new Token (INTEGER_LITERAL, locus, str);
  }

  static Token *
  make_real (location_t locus, const char *str)
  {
    return new Token (REAL_LITERAL, locus, str);
  }

  static Token *
  make_string (location_t locus, const char *str)
  {
    return new Token (STRING_LITERAL, locus, str);
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

struct Lexer
{
private:
  int real_get (int n);
  location_t get_current_location ();

  int get_input ();
  int get_input (int);

  int peek_input ();
  int peek_input (int);

  TokenId classify_keyword (const char *);

  Token *build_token ();

public:
  Lexer (const char *filename, FILE *input);
  ~Lexer ();

  const Token *peek_token ();
  const Token *peek_token (int);

  const Token *get_token ();
  const Token *get_token (int);

private:
  FILE *input;

  int current_line;
  int current_column;
  const struct line_map *line_map;

  static const int max_column_hint = 80;

  struct InputSource
  {
    FILE *input;
    InputSource (FILE *input_) : input (input_) {}
    int operator() () { return fgetc (input); }
  };
  InputSource input_source;
  buffered_queue<int, InputSource> input_queue;

  struct TokenSource
  {
    Lexer *lexer;
    TokenSource (Lexer *lexer_) : lexer (lexer_) {}
    Token *operator() ()
    {
      Token *t = lexer->build_token ();
      lexer->token_seq.push_back (t);
      return t;
    }
  };

  TokenSource token_source;
  buffered_queue<Token *, TokenSource> token_queue;

  std::vector<Token *> token_seq;
};
}

#endif // TINY_LEXER_H

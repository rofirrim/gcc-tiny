#ifndef TINY_LEXER_H
#define TINY_LEXER_H

#include "config.h"
#include "system.h"
#include "input.h"

#include "tiny-token.h"
#include "tiny-buffered-queue.h"

namespace Tiny
{

struct Lexer
{
private:
  location_t get_current_location ();

  void skip_input ();
  void skip_input (int);

  int peek_input ();
  int peek_input (int);

  TokenId classify_keyword (const char *);

  TokenPtr build_token ();

public:
  Lexer (const char *filename, FILE *input);
  ~Lexer ();

  const_TokenPtr peek_token ();
  const_TokenPtr peek_token (int);

  void skip_token ();
  void skip_token (int);

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
    TokenPtr operator() ()
    {
      return lexer->build_token ();
    }
  };

  TokenSource token_source;
  buffered_queue<std::tr1::shared_ptr<Token>, TokenSource> token_queue;
};
}

#endif // TINY_LEXER_H

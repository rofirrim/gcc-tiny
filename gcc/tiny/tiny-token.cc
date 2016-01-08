#include "tiny-token.h"

namespace Tiny
{

const char *
get_token_description (TokenId tid)
{
  switch (tid)
    {
#define TINY_TOKEN(name, descr)                                                \
  case name:                                                                   \
    return descr;
#define TINY_TOKEN_KEYWORD(x, y) TINY_TOKEN (x, y)
      TINY_TOKEN_LIST
#undef TINY_TOKEN_KEYWORD
#undef TINY_TOKEN
    default:
      gcc_unreachable ();
    }
}

const char *
token_id_to_str (TokenId tid)
{
  switch (tid)
    {
#define TINY_TOKEN(name, _)                                                    \
  case name:                                                                   \
    return #name;
#define TINY_TOKEN_KEYWORD(x, y) TINY_TOKEN (x, y)
      TINY_TOKEN_LIST
#undef TINY_TOKEN_KEYWORD
#undef TINY_TOKEN
    default:
      gcc_unreachable ();
    }
}

}

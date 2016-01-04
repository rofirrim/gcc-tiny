#ifndef TINY_CONTEXT_H
#define TINY_CONTEXT_H

#include "tiny-scope.h"
#include <vector>

namespace Tiny
{

struct Context
{
public:
  Scope &
  scope ()
  {
    return *(current_scope.back ());
  }

  Scope &
  get_top_level_scope ()
  {
    gcc_assert (!current_scope.empty ());
    return *(current_scope.front ());
  }

  void push_scope ();
  void pop_scope ();

  Context ();

private:
  std::vector<Scope *> current_scope;
  static int scope_id;
};

}

#endif // TINY_CONTEXT_H

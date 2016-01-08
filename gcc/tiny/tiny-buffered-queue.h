#ifndef TINY_BUFFERED_QUEUE_H
#define TINY_BUFFERED_QUEUE_H

#include <vector>

#include "config.h"
#include "system.h"

namespace Tiny
{

template <typename T, typename Source> struct buffered_queue
{
public:
  buffered_queue (Source &src) : source (src), start (0), end (0), buffer () {}

  T
  peek (int n)
  {
    gcc_assert (n >= 0);

    int num_queued_items = end - start;
    int num_items_required = n + 1;

    if (num_items_required > num_queued_items)
      {
	int num_items_to_read = num_items_required - num_queued_items;

	if (end + num_items_to_read > (int) buffer.size ())
	  {
	    // Resize the buffer 1.5X
	    int new_size = (buffer.size () + num_items_to_read);
	    new_size += (new_size >> 1);

	    std::vector<T> new_queue (new_size);
	    std::copy (buffer.begin () + start, buffer.begin () + end,
		       new_queue.begin ());
	    start = 0;
	    end = num_queued_items;

	    std::swap (buffer, new_queue);

	    gcc_assert (end + num_queued_items < (int) buffer.size ());
	  }

	for (int i = 0; i < num_items_to_read; i++)
	  {
	    buffer[end + i] = source ();
	  }
	end += num_items_to_read;
      }

    gcc_assert (0 <= start);
    gcc_assert (start <= end);
    gcc_assert (end <= (int) buffer.size ());

    gcc_assert (start + n < end);

    return buffer[start + n];
  }

  void
  skip (int n)
  {
    peek (n);

    for (int i = 0; i < (n + 1); i++)
      {
	// Clear the value
	buffer[start + i] = T();
      }

    start += (n + 1);

    gcc_assert (0 <= start);
    gcc_assert (start <= end);

    // Compact buffer if empty
    if (start == end)
      {
	start = end = 0;
      }
  }

private:
  Source &source;

  // designates the range [start, end) inside buffer
  int start;
  int end;
  std::vector<T> buffer;
};
}

#endif // TINY_BUFFERED_QUEUE_H


// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __javax_management_NotificationBroadcasterSupport$DispatchTask__
#define __javax_management_NotificationBroadcasterSupport$DispatchTask__

#pragma interface

#include <java/lang/Object.h>
extern "Java"
{
  namespace gnu
  {
    namespace javax
    {
      namespace management
      {
          class ListenerData;
      }
    }
  }
  namespace javax
  {
    namespace management
    {
        class Notification;
        class NotificationBroadcasterSupport;
        class NotificationBroadcasterSupport$DispatchTask;
    }
  }
}

class javax::management::NotificationBroadcasterSupport$DispatchTask : public ::java::lang::Object
{

public:
  NotificationBroadcasterSupport$DispatchTask(::javax::management::NotificationBroadcasterSupport *, ::gnu::javax::management::ListenerData *, ::javax::management::Notification *);
  void run();
private:
  ::gnu::javax::management::ListenerData * __attribute__((aligned(__alignof__( ::java::lang::Object)))) ldata;
  ::javax::management::Notification * notif;
public: // actually package-private
  ::javax::management::NotificationBroadcasterSupport * this$0;
public:
  static ::java::lang::Class class$;
};

#endif // __javax_management_NotificationBroadcasterSupport$DispatchTask__

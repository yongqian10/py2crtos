
#-------------------------------------------------------------------------------
#Task

#    setup() -> yield -> do task -> yield
#
#    1) yield process back to OS (volutary or by interrupt)
#      no yield during critical process
#
#    2) yield by call blocking api -> delay, waiting for msg / mutual exclusion lock especially high priority task
#
#    3) always yield, msg only delivered when task yield

class Task(func, priority=255, name=None, notifications=None, mailbox=False)
    pass
    # func must be a generator

add_task(task) # auto initialize task

start(scheduler=None) # never return function(blocking)


#-------------------------------------------------------------------------------
#Notification

#    notificaton vs message
#
#    a lightweight message to a task from service routine/ other task
#
#    have a state(8 bit/32 bit) and value(0/1)
#
#    task can yield while wait for a notification to be set to a particular value

#-------------------------------------------------------------------------------
#Message
#
#   task pass msg -> local task outgoing queue -> deliver during task yield -> other task mailbox
#
#   each msg only have 1 sender and 1 recipent
#
#   msg type -> pyRTOS.QUIT / user defined type(integer >128)
#
#   msg field -> can contain anything(list, object)
#
#   addressing
#       -> reference to target object
#                   vs
#       -> name of target object
#
#   check msg every loop to prevent system run out of memory, especially low priority task

task.deliver(<list of args>)


#-------------------------------------------------------------------------------
#Error Handling


#-------------------------------------------------------------------------------
#Interrupt


#-------------------------------------------------------------------------------
# Mutex & Sync

# priority based mutex
class Mutex():
    pass

    Mutex.lock(task) # blocking
    #   eg: yield [mutex.lock(self)]

    Mutex.nb_lock(task) # non-blocking lock

    Mutex.unlock()
    #   should be unlocked by same task that acquired the lock


# first come first serve mutex
class BinarySemaphore():
    pass

    BinarySemaphore.lock(task) # blocking
    #   eg: yield [BinarySemaphore.lock(self)]

    BinarySemaphore.nb_lock(task) # non-blocking

    BinarySemaphore.unlock()


#-------------------------------------------------------------------------------
#Service routine
#
#    define task priority
#    assign compute resource to task according to task priority

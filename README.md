
-------------------------------------------------------------------------------
Task

    setup() -> yield -> do task -> yield

    1) yield process back to OS (volutary or by interrupt)
      no yield during critical process

    2) yield by call blocking api -> delay, waiting for msg / mutual exclusion lock especially high priority task

    3) always yield, msg only delivered when task yield

-------------------------------------------------------------------------------
Notification

    notificaton vs message

    a lightweight message to a task from service routine/ other task

    have a state(8 bit/32 bit) and value(0/1)

    task can yield while wait for a notification to be set to a particular value

-------------------------------------------------------------------------------
Message

    task pass msg -> local task outgoing queue -> deliver during task yield -> other task mailbox

    each msg only have 1 sender and 1 recipent

    msg type -> pyRTOS.QUIT / user defined type(integer >128)

    msg field -> can contain anything(list, object)

    check msg every loop to prevent system run out of memory, especially low priority task

    ---------------------------------------------------------------------------
    Syntax

    task.deliver(<list of args>)


-------------------------------------------------------------------------------
Error Handling



-------------------------------------------------------------------------------
Service routine

    define task priority
    assign compute resource to task according to task priority

<h1 align=center>
    Twin
</h1>

<p align=center>
    The fastest way to develop native rtos app with python3
</p>

### Why is Twin awesome
- 🚀 **Quick start**: Due to its expressiveness, you can create and deploy a production-ready rtos app from scratch with very few lines of concise, consistent, declarative code.
- 😌 **No boilerplate**: By abstracting away complex c features, there is less boilerplate code. That means less code to maintain and understand! It also means easier upgrades.
- 🔓 **No lock-in**: You can deploy the Twin app to open-source rtos like FreeRtos and Zephr. There is no lock-in into specific providers; you have full control over the code (and can actually check it out in .twin/ dir if you are interested ).
-    **High-level debug**: Set your rules and let Twin held them for you, Twin support constraint such as timing and shared memory reference check.

### Features
 🔒 Command based tasks definition, 🖇️ Timing and shared memory validation, 🚀 Development with tasks overview, ⚙ ️All Python3

### Code example
Simple Twin config file in which you describe the high-level details of your Twin app:
```py
// file: main.twin

twinapp = [
    # common
    [
        addQueue('q1', 5)
    ],

    # task 1
    [
        addVar(buffer),
        QueueReceive('q1', buffer),
        printString(buffer)
    ],

    # task 2
    [
        addVar(buffer, "hello"),
        QueueSend('q1', buffer)
    ]
]
```

### How its works
-   **Debug**: Twin build a relationship graph to manage dependencies(event, task and shared memory)

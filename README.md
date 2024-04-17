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
-    **Debug**: Set your rules and let Twin held them for you, we support constraint such as timing and shared memory reference check.

### Features
 🔒 Command based tasks defination, 🖇️ Timing and shared memory validation, 🚀 Development with tasks(multiple list) overview, ⚙ ️All Python3

### Code example
Simple Wasp config file in which you describe the high-level details of your web app:
```js
// file: main.wasp

app todoApp {
  title: "ToDo App",  // visible in the browser tab
  wasp: { version: "^0.13.0" },
  auth: { // full-stack auth out-of-the-box
    userEntity: User, methods: { email: {...} }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true, // Limit access to logged-in users.
  component: import Main from "@client/Main.tsx" // Your React code.
}

query getTasks {
  fn: import { getTasks } from "@server/tasks.js", // Your Node.js code.
  entities: [Task] // Automatic cache invalidation.
}

entity Task {=psl  // Your Prisma data model.
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

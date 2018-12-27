This is a Webstorm plugin for Javascript deep-type-inference-based property completion and annotation.

As you may know, by default Webstorm suggests _any_ property ever defined in 
your project when you ask for completion in `someVar.` no matter in what context.

You may check the "Only type-based completion" flag in settings, but you will 
quickly notice that there is no completion in some places that could have it. 

For example, arguments of functions like `map`, `filter`, `reduce`, 
`sort` and functions defined by user won't get completion.

So, that's why I'm writing this plugin - to make it possible to code in js as comfortable as you 
would in some static-typed language with the help of type inference and doc-comments.

This plugin will be highly inspired by my other plugin, 
[deep-assoc-completion](https://plugins.jetbrains.com/plugin/9927-deep-assoc-completion) 
and most features will be borrowed from there

![image](https://user-images.githubusercontent.com/5202330/50491395-c90cd680-0a1a-11e9-9510-fa996c8924ed.png)
Infer property list in a `Promise.then` inside `map` from result of a function defined in other module.

![](https://user-images.githubusercontent.com/5202330/50492068-28201a80-0a1e-11e9-946f-7525aebd59ca.png)
Take args passed to the function into account.

![image](https://user-images.githubusercontent.com/5202330/50492169-c01e0400-0a1e-11e9-9eff-44d2cfebe09b.png)
Infer anonymous function arg type based on what it is called with inside another function.

![image](https://user-images.githubusercontent.com/5202330/50492329-a4ffc400-0a1f-11e9-93dd-2cc3a5ea6fa2.png)
Infer function arg type based on usage in current file (great for private helper functions you define to split code).

![image](https://user-images.githubusercontent.com/5202330/50492452-5acb1280-0a20-11e9-93f7-75ff4308daa1.png)
Reference module or a var in jsdoc comment.
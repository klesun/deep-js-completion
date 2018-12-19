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

Also I guess parsing query in `document.querySelector('input.midi-file')` will be useful 
to suggest fields specific to the selected tag like `value` for an `<input/>`. I bet there is a 
ton of other js-specific things I always wanted, but did not have a a plugin to put the logic into...
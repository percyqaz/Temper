# What's going on here?

**String templating** is when you have some data, and by writing a **template** you describe how to transform that data into a piece of text. There are tons of uses for this, but especially code generation in my case.

This library is my take on string templating, with the catch that **templates can also be used to parse pieces of text, and re-extract the data**.

### My main goals with this:

1. A simplified alternative to regular expressions, that can generate actual regular expressions suitable for putting in your favourite IDE's Find+Replace tool. Most Find+Replace tools allow regex support not only for matching text, but you can grab portions of the match out and dynamically replace the text. The advantage of generating them is that you can write a more readable/memorable template with meaningful variable names.

2. Code generation. T4 templates (and similar templating projects I found) are very powerful, but because of this only go from data -> text. Part of the simple design for this project is to allow for reading a code file, changing a few bits of data, and writing back.

### To-do list:
- Build a few more of the features I have in my head
- Write tests
- Write documentation

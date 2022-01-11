# What's going on here?

**String templating** is when you have some data, and by writing a **template** you describe how to transform that data into a piece of text. There are tons of uses for this, but especially code generation in my case.

This library is my take on string templating, with the twist that **templates can also be used as text parsers, and re-extract the data**.
Ultimately, templates give you an interface between pieces of text and the **information contained in that text**, which you can use for intelligent find-and-replace, code generation, etc.

Most of the concepts/design decisions are similar to [Liquid Templates](https://shopify.github.io/liquid/) (some by coincidence, some by inspiration) so documentation will probably be explaining things through that lens --
Liquid templates are not designed to be used as parsers, which is where Temper differs

### Milestone list:
#### Tests
- [x] Have some simple tests
- [ ] Full reading tests
- [ ] Full writing tests
- [ ] Round trip tests
- [ ] Template warning tests
#### Documentation
- [ ] Have a wiki page
- [ ] Document syntax
- [ ] Document API
- [ ] Examples library
#### Functionality
- [x] Basic read-write templating
- [x] Warning codes
- [x] Data types & Data lenses
- [ ] Sub-templates/meta template stuff
- [ ] Integration with Percyqaz.Json for mapping template data to/from JSON text and .NET types

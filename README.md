# ShinyTester
An R package to help debug Shiny apps during the process itself.

### To install:
`devtools::install_github("mexindian/ShinyTester")`

### Motivation

** TL;WR VERSION:** Shiny is awesome but a bit daunting and easy to make mistakes in. These functions should help make it a bit less daunting.


I came back to Shiny after a hiatus of a few years and it was much more challenging than I feel comfortable admitting. I was comitting bonehead moves like writing `something` instead of `output$something`, confusing where to put `Output` commands vs `Render` commands, etc. I would eventually find my mistake, curse myself for my terrible memory and move on with a crumpled ego. Then I had the realization that maybe if I was a beginner, I wouldn't even know what I was doing wrong.

At the same time I kind of thought to myself as I was developing a Shiny App "I really should draw a hirearchy for this crap... I can't keep it straight!" What I find especially complex about Shiny is that the functions that are fed in, for example, reading a `reactive` block into a dataframe, is that they are not fed in through parameters as in a normal function, they are fed into the script in the body itself... which makes it harder to check that every block has all the stuff it needs. 

These two thoughts culminated in two functions that analyze the code itself (I guess we should then call these _metafunctions_), one to check how items are created in `server.R` and then how they are called in `ui.R` with some fairly naive checks put on, and another to create an _ad hoc_ hirearchy of the Shiny Server. It is my hope that both of these combined minimize the intrinsic boneheadedness of us all.

### Examples

ToDo

### Caveats:
This is a very naive app... it works best with my style of programming and will probably take significant work to universalize (since we're talking about code... maybe it's impossible to fully universalize). Please check the notes on this (and also a few other caveats):
 - For now only works with `<-` assignments, not `=` or `->` assignments
 - For now calling items only works with doublequotes. (ie. `plotOutput("thingie")` works, `plotOutput('thingie')` doesn't.
 - For now, only supports seperate ui.R and server.R Shiny apps... the single `app.R` implementation is not supported.
 - `isolate` and `observe` are not supported yet
 - For now, I don't read in data outside the shinyserver (for example, if I want to pass data in that only needs to be calculated once. Not sure yet what's the best way.
 

[![Travis-CI Build Status](https://travis-ci.org/mexindian/ShinyTester.svg?branch=master)](https://travis-ci.org/mexindian/ShinyTester)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ShinyTester)](https://cran.r-project.org/package=ShinyTester)
[![Downloads](https://cranlogs.r-pkg.org/badges/ShinyTester)](https://cran.r-project.org/package=ShinyTester) 
# ShinyTester
An R package to help debug Shiny apps during the process itself.

### To install:
`install.packages("ShinyTester")` 

(dev version `devtools::install_github("mexindian/ShinyTester")`)
### Motivation

** TL;WR VERSION:** Shiny is awesome but a bit daunting and easy to make mistakes in. These functions should help make it a bit less daunting.


I came back to Shiny after a hiatus of a few years and it was much more challenging than I feel comfortable admitting. I was comitting bonehead moves like writing `something` instead of `output$something`, confusing where to put `Output` commands vs `Render` commands, etc. I would eventually find my mistake, curse myself for my terrible memory and move on with a crumpled ego. Then I had the realization that maybe if I was a beginner, I wouldn't even know what I was doing wrong.

At the same time I kind of thought to myself as I was developing a Shiny App "I really should draw a hirearchy for this crap... I can't keep it straight!" What I find especially complex about Shiny is that the functions that are fed in, for example, reading a `reactive` block into a dataframe, is that they are not fed in through parameters as in a normal function, they are fed into the script in the body itself... which makes it harder to check that every block has all the stuff it needs. 

These two thoughts culminated in two functions that analyze the code itself (I guess we should then call these _metafunctions_):
 - `ShinyDummyCheck()` - to check how items are created in `server.R` and then how they are called in `ui.R` with some fairly naive checks put on, and 
 - `ShinyHierarchy()` - to create an _ad hoc_ hirearchy of the structure of the Shiny Apps - ie - what inputs go to what reactives, what reactives go to other reactives, and what then gets pushed back out to the UI as an output. 
 
It is my hope that both of these combined minimize the intrinsic boneheadedness in us all. This is really quite alpha though... please do check the Caveats!

### Examples for `ShinyDummyCheck`:

```
ShinyTester::ShinyDummyCheck("https://raw.githubusercontent.com/mexindian/ShinyServer/master/LineSelector")
```

Provides this table:

|Item       |SrvCall         |isOutput |VisualCall         |Status |
|:----------|:---------------|:--------|:------------------|:------|
|a          |reactive        |NA       |NA                 |OK     |
|FinalDF    |reactive        |NA       |NA                 |OK     |
|Plot3      |renderPlotly    |Yes      |plotlyOutput       |OK     |
|PERC       |renderText      |Yes      |verbatimTextOutput |OK     |
|fig1       |renderPlot      |Yes      |plotOutput         |OK     |
|figControl |renderPlot      |Yes      |plotOutput         |OK     |
|figControl |renderPlot      |Yes      |plotOutput         |OK     |
|table1     |renderDataTable |Yes      |dataTableOutput    |OK     |

Which shows that there are no errors in the Shiny app, oh except for the fact that I defined an object twice... whoops (Yeah, see that's exactly the boneheadedness I'm talkin bout). The structure of the table is as follows:
- Item - The name of the asset that maybe should be on both server.R and ui.R
- SrvCall - the TYPE of object that you're saying this specific item is (in server.R)
- isOutput  - is a binary that will specify if in server.R you wrote just `item` or `output$item`
- VisualCall - is the TYPE of thingie you're trying to push the item into (in ui.R). 
- Status - Compares the SrvCall to the VisualCall, also looks at isOutput and then applies some rules to figure out if it's probably ok or not. 

### Examples for `ShinyHierarchy`:

A simple example:
```
library(ShinyTester)
ShinyHierarchy("https://raw.githubusercontent.com/rstudio/shiny-examples/master/003-reactivity")
```
Will yield:

![image](https://cloud.githubusercontent.com/assets/8094091/21746544/7830f6b2-d50e-11e6-8583-c90670786adc.png)

Which shows one of the weaknesses of the function... it assumes all Item names are unique... and will act strangely if this assumption doesn't hold (ie - caption).

A more complex example:
```
ShinyTester::ShinyHierarchy("https://raw.githubusercontent.com/mexindian/ShinyServer/master/LineSelector")
```
Yields: 

![image](https://cloud.githubusercontent.com/assets/8094091/21746698/169dcdc0-d514-11e6-88ed-357d37293b65.png)

And here we can start to see the structure that I'm attempting to show... there are basically three ROWS of nodes. The first one is the UI Inputs, the second row are the reactives (kinda...), and the third row are the outputs being visualized. I said the reactives are "kinda" the second row because I have introduced a small shift to each node in the middle row in order to see reactive flows into each other (if they are all in the same row, you can't really see them). The structure is made evident in a more complex case below (forgive the redacted names):

![image](https://cloud.githubusercontent.com/assets/8094091/21746742/67a21a86-d515-11e6-96d4-5456b54a7747.png)


### Caveats:
This is a very naive app, and in early stages at that... it works best with my style of programming and will probably take significant work to universalize (since we're talking about code... maybe it's impossible to fully universalize).  Some other caveats:
 - For now only works with `<-` assignments, not `=` or `->` assignments
 - ~~For now calling items only works with doublequotes. (ie. `plotOutput("thingie")` works, `plotOutput('thingie')` doesn't.~~
 - For now, only supports seperate ui.R and server.R Shiny apps... the single `app.R` implementation is not supported.
 - `isolate` and `observe` are not supported yet
 - For now, I don't read in data outside the shinyserver call (for example, if I want to pass data in that only needs to be calculated once. Not sure yet what's the best way.
 - For now it only analyzes the main scripts, if you are SOURCEing files in from other places, it won't work.
 
 ### Other tips for working in Shiny:
 - Add to your server.R and ui.R TEST items. for example, add one for a data.frame and one for a figure. (ADD CODE). You can keep these commented out or displaying random data... then, when you add a new element, just test them in the test blocks before adding them to the exact place. Saves time.
 - Likewise, during testing, if you need to run through the code to debug, you can always simulate inputs by writing this: `input <- data.frame(Parameter1="thingie1",Parameter2="thingie2")`. Keep this commented out, but when you test, you can run through the Shiny app as if it were live.
 - Check Dean Attali's excellent tips and tricks (http://deanattali.com/blog/advanced-shiny-tips/).

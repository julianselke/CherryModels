# &#127826;CherryModels

The <b>CherryModels</b> package facilitates computing thousands of linear mixed-effects models to determine the model that best explains your data. To this end, all combinations of fixed effects with and without interaction are generated and computed with `lmerTest::lmer()`. Subsequently, a step-wise model reduction is performed on every model with `lmerTest::step()`.  

Installation & introduction:

```
library(devtools)
install_github("julianselke/CherryModels")

vignette("CherryModels")
```

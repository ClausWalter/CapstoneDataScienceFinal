Data Science Capstone - Word Prediction App "Sparrow"
========================================================
author: Claus Walter
date: 08.06.2017
autosize: true
<div align="center">
<img src="sparrow.png" width=200 height=250>
</div>

Introduction
========================================================
Sparrow is a R/Shiny-based text prediction app. Special features:

- It learns abou you - it remembers what you entered and will use that for future predictions!
- It is fast - although there is certainly a trade-off between speed and accuracy
- It can handle "odd" input values like e.g. things you would find in tweets or blogs. So not necessarily Oxford-English ;-)
- It shows the data it uses for predictions, so you can see it "think" - in a certain way
- It is small - less than 30 MB, so it can squeeze into almost any device
- It finds a swearword it knows, it just shows ***** - so it behaves nicely


How Sparrow works - Algorithm and Implementation
========================================================
Sparrow uses a variant of "stupid back-off algorithm":

1. Take input, cut into words, stem etc.

2. Search in the n-gram tables for matches to the input

3. Assign matches weights based on frequency, n-gram table of origin etc.

4. Full search results are displayed in tab "Prediction History and Details" in a table, along with table of origin etc.

5. Final step: best fit (with the hightest weight) gets unstemmed etc. and suggested as prediction


How to use Sparrow
========================================================

Sparrow follows a very simple approach, by following these steps (as also shown in the picture):

1. Enter text

2. Press "Predict" button and check prediction result

3. Take prediction over (paste) into input field (if you want)
<div align="center">
<img src="screenshot.png" width=400 height=135>
</div>
Many more details under the tabs "How to use" and "Advanced Documentation" inside the application.

References
========================================================
Here the references to the coding, the presentation and the app:

Sparrow application: <https://clauswalter.shinyapps.io/DataScienceCapstoneApplication/>

Github: <https://github.com/ClausWalter/CapstoneDataScienceFinal>

This presentation: <http://rpubs.com/ClausWalter/Capstone>

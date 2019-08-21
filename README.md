# Exchange_Rate_Prediction_Shiny

## Review

Exchange Rate between INR and USD (Indian Rupee and the United States Dollar) changes rapidly nowadays. Due to the equaility of trade, the Pricing strategy has to include this changes. 

## Model Interpretation 

A local polynomial regression has been used in this Web App. Due to the rate's variability, LSTM is not that usefull in this model(**My Fault!!** LSTM is a good model). Choosing the LOESS can fit the rate in a very short interval (one week to one month). 

Issue and Pull Request is welcomed

## To Do

- [ ] Delete Date widget (Previous one month data predicitng the next one data)
- [ ] Adding multiple currency (Type in currency's acronym and get the output)
- [ ] Multiple Modeling methods (LSTM, ANN, etc.)
- [ ] Fansy UI with CSS 

## Reference
https://github.com/wuthmone/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model

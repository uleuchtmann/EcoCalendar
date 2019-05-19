# EcoCalendar

This creates a calendar of upcoming economic data releases 
for a specific period (e.g. the next 24 hours). 

The list of potential economic data releases is located in 
'ecoitems.csv'. 
Every line in 'ecoitems.csv' should hold 5 fields, separated by commas (,): 

1. the full bloomberg ticker, e.g. 'CPI YOY Index';
2. the English-language item description, e.g. 'CPI';
3. the English-language unit description, e.g. '%yoy';
4. the German-language item description, e.g. 'Konsumentenpreise';
5. the German-language unit description, e.g. '% Vj.'.


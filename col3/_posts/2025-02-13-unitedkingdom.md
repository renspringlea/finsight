---
layout: post
title: "United Kingdom"
permalink: "/unitedkingdom/"
---
# Production (2022)  

  <table class='prodtable'>
  {% for row in site.data.unitedkingdom_production %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>
<div class='prodtablenotes'>
Table notes: harvest weight, harvest age, and mortality rate are set by us as biological parameters (see bottom of page for details). Production is then used, with these parameters, to calculate individuals slaughtered, individuals hatched, and individuals inventory. 'Inventory' refers to the number of fish alive on animals at any one time. Production weight is rounded to the nearest tonne.
Biological parameters explained and cited <a href="{% link parameters.md %}">here</a>. Note that some biological parameters for less frequently farmed species are placeholder values.
</div>


![time series of individuals slaughtered over time](../assets/images/unitedkingdom_timeseries.png)

# Trade (2023)  
## Exports  


  <table>

  {% for row in site.data.unitedkingdom_export %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>
## Imports  

  <table>
  {% for row in site.data.unitedkingdom_import %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>


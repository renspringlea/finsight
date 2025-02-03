---
layout: home
---

This website presents, in a format convenient for fish welfare advocates, key industry and economic data relating to fish farming in Europe. Data is automatically updated monthly.

# Production
![map_production.png]({{ site.url }}assets/images/map_production.png)  

<table>
  {% for row in site.data.prodtable %}
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

# Trade
![map_trade_flow.png]({{ site.url }}assets/images/map_trade_flow.png)  

# Retail prices

<table>
  {% for row in site.data.eu_retail_aggregate %}
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

We have found two additional sources of price information that may be of interest.  
1. UK retail prices from the Seafish report ["Farmed seafood in multiple retail"](https://www.seafish.org/insight-and-research/seafood-retail-data-and-insight/) reports average prices for March 2024 in GBP/kg: salmon = 20.53; warm water prawns = 17.18; sea bass = 16.25; sea bream = 15.23; shrimps = 16.19; tilapia 10.75.  
2. Türkiye producer prices from Turkish Statistical Institute database [Fishery Statistics](https://biruni.tuik.gov.tr/medas/?kn=97&locale=en) (Price of aquaculture products). However, due to the ongoing Turkish currency crisis, producer prices are experiencing drastic changes that are challenging to interpret in a meaningful way (e.g. distinguishing between currency value changes and the change in value of specific fish species).  
3. Annual Norway price data can be downloaded from the [Norwegian Directorate of Fisheries](https://www.fiskeridir.no/English/Aquaculture/Statistics/Atlantic-salmon-and-rainbow-trout/grow-out-production)
4. Quartlerly UK (Scottish) price data can be download from [Salmon Scotland](https://www.salmonscotland.co.uk/reports) (see "Economic Quarterly" reports)

# Countries

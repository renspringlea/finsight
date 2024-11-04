---
layout: home
---

This website presents, in a format convenient for fish welfare advocates, key industry and economic data relating to fish farming in Europe. Data is automatically updated monthly.

# Production
![map_production.png]({{ site.url }}assets/images/map_production.png)  

<table align="center">
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


# Countries

---
layout: home
---
navigate:  
- [production](#production)
- [trade](#trade)
- [retail-prices](#retail-prices)
- [about](#about)
- [country-pages](#country-pages)

# production
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

# trade
![map_trade_flow.png]({{ site.url }}assets/images/map_trade_flow.png)  

# retail-prices

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


# about
This page presents, in a format convenient for fish welfare advocates, key industry and economic data relating to fish farming in Europe. Data is automatically retrieved, processed, and updated monthly. All data is from public sources.  

Contact: info {at} animalask {dot} org  

Made with ❤︎ by Ren Ryba (they/them) (Animal Ask).  
I live and work on the unceded land of the Kaurna people.  

The content on this page is licensed under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en).

Production data is processed and analysed based on original data from [FAO](https://www.fao.org/fishery/en/collection/aquaculture?lang=en) (used under license [CC BY-NC-SA 3.0 IGO](https://creativecommons.org/licenses/by-nc-sa/3.0/igo/deed.en)).
EU trade and price data is processed and analysed based on original data  from [EUMOFA](https://eumofa.eu/bulk-download) (used under license [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)).

~~~
|\   \\\\__     o
| \_/    o \    o
> _   ((    <_ o  
| / \__+___/      
|/     |/
~~~

# country-pages

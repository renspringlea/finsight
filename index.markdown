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

~~~
|\   \\\\__     o
| \_/    o \    o
> _   ((    <_ o  
| / \__+___/      
|/     |/
~~~

# country-pages

---
layout: home
---

# production
![g_production_individuals.png]({{ site.url }}assets/images/g_production_individuals.png)
![g_production_individuals2.png]({{ site.url }}assets/images/g_production_individuals2.png)
[download data]({{ site.url }}downloads/production_individuals.csv)  


![g_production_weight.png]({{ site.url }}assets/images/g_production_weight.png)
![g_production_weight2.png]({{ site.url }}assets/images/g_production_weight2.png)
[download data]({{ site.url }}downloads/production_weight.csv)  


![g_production_value.png]({{ site.url }}assets/images/g_production_value.png)
![g_production_value2.png]({{ site.url }}assets/images/g_production_value2.png)
[download data]({{ site.url }}downloads/value.csv)  

# retail prices

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

# trade
trade, for each species:
a huge graph showing arrows from country to country



# about
finsight is an online dashboard that provides access to key industry and economic data relating to fish farming in Europe, presented in a way convenient for fish welfare advocates. Data is automatically retrieved, processed, and updated monthly. All data is from public sources.  

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

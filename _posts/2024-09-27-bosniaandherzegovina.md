---
layout: post	itle: "Bosnia and Herzegovina
---
# Production (2022)  
![time series of individuals slaughtered over time](/assets/images/bosniaandherzegovina_timeseries.png)


  <table>
  {% for row in site.data.bosniaandherzegovina_production %}
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

# Trade (2023)  
## Exports  


  <table>

  {% for row in site.data.bosniaandherzegovina_export %}
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
  {% for row in site.data.bosniaandherzegovina_import %}
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


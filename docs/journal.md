---
---

There are some journal entries
==============================

See them here:

{% assign sorted_entries = site.journal_entries | sort | reverse %}
{% for j in sorted_entries %}
- [{{ j.date | date: "%Y-%m-%d" }} - {{ j.title }}]({{ j.url | relative_url }})
{% endfor %}

{% comment %}
Reference doc about the `sort` filter:
    https://jekyllrb.com/docs/liquid/filters/

Reference doc about the `date: "%Y..."` filter: (linked from reference above)
    https://shopify.github.io/liquid/filters/date/
{% endcomment %}


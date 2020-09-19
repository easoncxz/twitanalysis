
{% comment %} 
Comment tags are available like this.
    Reference doc:
        https://shopify.github.io/liquid/tags/comment/

The `{% link %}` tag:
    Reference doc:
        https://jekyllrb.com/docs/liquid/tags/#linking-to-posts

    Breaking changes in Jekyll 4.0:
        Forum:
            https://talk.jekyllrb.com/t/relative-url-and-baseurl/2051

        Official notice:
            https://jekyllrb.com/docs/liquid/tags/#linking-to-posts

        Summary:
            > Since Jekyll 4.0 , you don’t need to prepend link and post_url tags with site.baseurl.

{% endcomment %}

Head over to the list of journals in the [journal]({{ site.baseurl }}{% link journal.md %}) page.

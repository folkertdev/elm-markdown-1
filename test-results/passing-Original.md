# Original

## auto_links

### Example undefined

This markdown:


````````````markdown
Link: <http://example.com/>.

With an ampersand: <http://example.com/?foo=1&bar=2>

* In a list?
* <http://example.com/>
* It should.

> Blockquoted: <http://example.com/>

Auto-links should not occur here: `<http://example.com/>`

	or here: <http://example.com/>

````````````

Gives this correct output:


````````````html
<p>Link: <a href="http://example.com/">http://example.com/</a>.</p>

<p>With an ampersand: <a href="http://example.com/?foo=1&amp;bar=2">http://example.com/?foo=1&amp;bar=2</a></p>

<ul>
<li>In a list?</li>
<li><a href="http://example.com/">http://example.com/</a></li>
<li>It should.</li>
</ul>

<blockquote>
  <p>Blockquoted: <a href="http://example.com/">http://example.com/</a></p>
</blockquote>

<p>Auto-links should not occur here: <code>&lt;http://example.com/&gt;</code></p>

<pre><code>or here: &lt;http://example.com/&gt;
</code></pre>

````````````

## blockquotes_with_code_blocks

### Example undefined

This markdown:


````````````markdown
> Example:
> 
>     sub status {
>         print "working";
>     }
> 
> Or:
> 
>     sub status {
>         return "working";
>     }

````````````

Gives this correct output:


````````````html
<blockquote>
  <p>Example:</p>

<pre><code>sub status {
    print "working";
}
</code></pre>
  
  <p>Or:</p>

<pre><code>sub status {
    return "working";
}
</code></pre>
</blockquote>

````````````

## code_blocks

### Example undefined

This markdown:


````````````markdown
	code block on the first line
	
Regular text.

    code block indented by spaces

Regular text.

	the lines in this block  
	all contain trailing spaces  

Regular Text.

	code block on the last line

````````````

Gives this correct output:


````````````html
<pre><code>code block on the first line
</code></pre>

<p>Regular text.</p>

<pre><code>code block indented by spaces
</code></pre>

<p>Regular text.</p>

<pre><code>the lines in this block  
all contain trailing spaces  
</code></pre>

<p>Regular Text.</p>

<pre><code>code block on the last line
</code></pre>

````````````

## horizontal_rules

### Example undefined

This markdown:


````````````markdown
Dashes:

---

 ---
 
  ---

   ---

	---

- - -

 - - -
 
  - - -

   - - -

	- - -


Asterisks:

***

 ***
 
  ***

   ***

	***

* * *

 * * *
 
  * * *

   * * *

	* * *


Underscores:

___

 ___
 
  ___

   ___

    ___

_ _ _

 _ _ _
 
  _ _ _

   _ _ _

    _ _ _



Not horizontal rules:

--*

-*-

*--

 -_-

  __-

   -__

    _-_


Long rules:

-----------

___________

***********

````````````

Gives this correct output:


````````````html
<p>Dashes:</p>

<hr />

<hr />

<hr />

<hr />

<pre><code>---
</code></pre>

<hr />

<hr />

<hr />

<hr />

<pre><code>- - -
</code></pre>

<p>Asterisks:</p>

<hr />

<hr />

<hr />

<hr />

<pre><code>***
</code></pre>

<hr />

<hr />

<hr />

<hr />

<pre><code>* * *
</code></pre>

<p>Underscores:</p>

<hr />

<hr />

<hr />

<hr />

<pre><code>___
</code></pre>

<hr />

<hr />

<hr />

<hr />

<pre><code>_ _ _
</code></pre>

<p>Not horizontal rules:</p>
<p>--*</p>
<p>-*-</p>
<p>*--</p>
<p>-_-</p>
<p>__-</p>
<p>-__</p>
<pre><code>_-_
</code></pre>
<p>Long rules:</p>
<hr>
<hr>
<hr>

````````````

## inline_html_comments

### Example undefined

This markdown:


````````````markdown
Paragraph one.

<!-- This is a simple comment -->

<!--
	This is another comment.
-->

Paragraph two.

<!-- one comment block -- -- with two comments -->

The end.

````````````

Gives this correct output:


````````````html
<p>Paragraph one.</p>

<!-- This is a simple comment -->

<!--
    This is another comment.
-->

<p>Paragraph two.</p>

<!-- one comment block -- -- with two comments -->

<p>The end.</p>

````````````

## nested_blockquotes

### Example undefined

This markdown:


````````````markdown
> foo
>
> > bar
>
> foo

````````````

Gives this correct output:


````````````html
<blockquote>
  <p>foo</p>
  
  <blockquote>
    <p>bar</p>
  </blockquote>
  
  <p>foo</p>
</blockquote>

````````````


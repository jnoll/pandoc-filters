---
header-includes:
    - \usepackage{color}
    - \usepackage{mdframed}
...

# This header is not orange { .color color="blue" }

Here is some text.  _This emph text should be colored red._  
<span class="color" color="green">This span text should be green.</span>
<span class="bgcolor" bgcolor="cyan">This span of text should have a cyan background.</span>
<span class="color bgcolor"  bgcolor="blue" color="white">This span of text should have white
text on a blue background.</span>
Here is the last text.

The  following text should be red: 
[This text should be red.]{.color color="red"}
<span class="color" color="red">This text should be red.]{.color color="red"}</span>

# This header is not colored

<div class="bgcolor color frame" color="blue" bgcolor="cyan">
This block

of paragraphs

made by a div

is blue on cyan.
</div>

<div class="bgcolor color">
This 

is

the  default

color block.
</div>

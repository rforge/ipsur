<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
  <title>IPSUR Downloads</title>
  <link rel="Shortcut Icon" href="favicon.ico"><style type="text/css">
  <!--
    @import url("main.css");
    body { background-image: url();}
  -->
  </style>
</head>

<body>
  <div align="center">
    <!-- All of the page content is in this table-->
    <table width="700" height="100%" border="0" cellpadding="0" cellspacing="0">
      <tr>
        <td align="left" valign="top" bgcolor="#FFFFFF">

<!--BEGIN: Page Banner-->
<?php
  include "banner.php";
?>
<!--END: Page Banner-->
	
<!--BEGIN: Top Menu-->
<?php
  include "topMenu.php";
?>
<!--END: Top Menu-->
	
<!--BEGIN: Page Content-->
<p class="articleTitle">
  <strong>Downloads for the <span class="name">IPSUR</span> package</strong>
</p>

<p>
This page has the source code for the released version of IPSUR in a variety of formats.
</p>

<p>
You can get a PDF copy of the published edition of <span class="name">IPSUR</span> from CRAN, and you can get a PDF of the development edition from R-Forge;  see the the <a href="installation.php">Installation</a> page for details.  Note that the Sweave sources of both of those documents are stored alongside their PDF siblings.
</p>

<p>
If you would like to print your own copy of the <strong>published</strong> edition of <span class="name">IPSUR</span> then you may want to use the following publication-ready PDF of the book.  It has been typeset in black-and-white to optimize the display of the code and figures. All of the fonts have been embedded, so the printer should not have trouble with any of the symbols. 
</p>

<p>
You can try to print your own copy of the <em>development</em> version of <span class="name">IPSUR</span>, but I do not recommend it for several reasons.  First of all, the electronic copy is in color which does not show up well on black-and-white paper.  Second of all, the electronic copy is optimized for web viewing which means that not all of fonts are embedded - your printer may not render all of the symbols correctly.  Finally, unless you have a very fancy printer you will be printing only on one side of the page - which wastes paper.
</p>

<p>
If you insist on printing your own copy of the development version then I recommend you download the LaTeX source (see below), compile your own PDF in black-and-white, and make sure to embed all of the fonts.
</p>

<p>
If you would like a transparent copy of the published edition of <span class="name">IPSUR</span>, then download the following .zip file.  It contains all of the LaTeX source code, all of the figures (in .pdf and .eps), and the ancillary materials such as IPSUR.R and IPSUR.RData.  This document can be used (in principle) to generate your own personal copy of the published edition of <span class="name">IPSUR</span>, and you are FREE to modify and distribute your own version of it under the constraints of the GNU-FDL.
</p>

<p>
Finally, you can download the LyX source file, IPSUR.lyx, which was used by the author to write <span class="name">IPSUR</span>. It is essentially the Sweave source code for the published edition, except that LyX conceals the intermediate step of generating the LaTeX source file and figures.  This one text file will generate a PDF of the entire published edition of <span class="name">IPSUR</span>, with a single click.  Enjoy.  (I certainly have.)
</p>

<!--END: Page Content-->

<!--BEGIN: Footer-->
<p align="center">
  R logo &copy; the R Foundation, 
    <font face="Courier New">
      <a href="http://www.r-project.org" style="text-decoration: none">
        <span style="text-decoration: none">http://www.r-project.org</span>
      </a>
    </font>
</p>
<!--END: Footer-->
  
<!--Close main table-->
      </td>
    </tr>
  </table>
</div>
</body>
</html>

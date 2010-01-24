<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
  <title>IPSUR Installation</title>
  <link rel="Shortcut Icon" href="favicon.ico"><style type="text/css">
  <!--
    @import url("main.css");
      body {
        background-image: url();
	}
    -->
  </style>
</head>

<body>
  <div align="center">
    <table width="700" height="100%" border="0" cellpadding="0" cellspacing="0">
      <tr>
        <td align="left" valign="top" bgcolor="#FFFFFF">
	<!-- All of the page content is in this table-->

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
<p class="articleTitle">Installation Instructions</p>
<blockquote>
      
<div id="list">
	<ol type="1">
		<li>
		  <strong>Download the latest version of <span class="name">R</span></strong>: choose a link 
			below to download the latest version of R for your operating system from CRAN:
			  
		  <div align="center">
		    <br />
	              Windows - <a href="http://cran.r-project.org/bin/windows/base/">http://cran.r-project.org/bin/windows/base/</a>
				<br />
                      MacOS X - <a href="http://cran.r-project.org/bin/macosx/">http://cran.r-project.org/bin/macosx/</a>
				<br />
                      Linux - 	<a href="http://cran.r-project.org/bin/linux/">http://cran.r-project/bin/linux</a>
		  </div>	
			<ul><br />  
				<li>
		  		<strong>Windows Installation Tip for <span class="name">R</span></strong>:  click the 
				<span class="name">.exe</span> 
				program file to start installation.  When it asks for "Customized startup options", 
				specify <em>Yes</em>.  In the next window, be sure to select the SDI (single-window) option.
				</li>
			</ul>
		</li>
		<li>
		  <strong>Install the <span class="name">IPSUR</span> package</strong>: download and install 
			the <span class="name">IPSUR</span> package from one of the following locations.
			The choice of location will depend on which version of <span class="name">IPSUR</span> 
			that you want to read. 
                    <br /><br />
			<ul>
				<li>
				<strong>From CRAN:</strong> choose this location if you would like an
				electronic copy of the most recently published edition of <span class="name">IPSUR</span>.
				This version may differ slightly from a hard copy purchased from retailers, but will be
				the closest you can get to a published copy without buying one.
				To install directly from CRAN, launch R and type the following at the command prompt "&gt;": 
				<br />
			    	<div align="center"><br />
					<span class="name">install.packages("IPSUR", repos="http://cran.r-project.org")</span>
			    	</div>
				</li>
				<li>
				<strong>From R-Forge:</strong> choose this location if you would like an 
				electronic copy of the next, yet-unpublished edition of <span class="name">IPSUR</span>. 
				This version will have the latest corrections and additional material, and may differ
				substantially from one purchased from retailers. To install directly from R-Forge, launch 
				R and type the following at the command prompt "&gt;": 
				<br />
			    	<div align="center"><br />
					<span class="name">install.packages("IPSUR", repos="http://R-Forge.R-project.org")</span>
			    	</div>
				</li>
			</ul>
		</li>
		<li>
			<strong>Load the <span class="name">IPSUR</span> package: </strong>
			Once <span class="name">IPSUR</span> is downloaded and installed, it must be loaded into R. 
			To do this type the following at the command prompt &quot;&gt;&quot;: 	
			<div class="name" align="center"><br />
                       		library(IPSUR)<br />
                       		read(IPSUR)<br />
                    	</div>
		</li>
	</ol>
</div>
	
</blockquote>
<!--END: Page Content-->

<!--BEGIN: Footer-->
 <p align="center">
	  R logo &copy; the R Foundation, <font face="Courier New"><a href="http://www.r-project.org" style="text-decoration: none"><span style="text-decoration: none">http://www.r-project.org</span></a></font>
	  </p>
<!--END: Footer-->

		<!--Close main table-->
		</td>
	</tr>
</table>
</div>
</body>
</html>

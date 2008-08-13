<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
	<title>IPSUR Project</title>
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
<p class="articleTitle">
              <strong>Download the IPSUR Package</strong>
            </p>
<blockquote>
<p>
                The current released version of the IPSUR package is 0.1-4. There are binary packages available for computers running Windows and MacOS X, and source code for Linux distributions. Downloads for the released version are from CRAN, the Comprehensive R Archive Network. 
                
    
	<p>
              </p>
    <div align="center">
		<strong>IPSUR Released Version 0.1-4 </strong>
		<br /><a href="http://cran.r-project.org/web/packages/RcmdrPlugin.IPSUR/index.html">CRAN IPSUR Package</a>
	</div>
    
	<p>
                <br />
                 The current development version of the IPSUR package is 0.1-5. This contains very early code for new features and is not suitable for production environments. The development version should be used for testing purposes only. Development downloads are from the project page hosted at R-Forge. </p>
    <div align="center">
		<strong>IPSUR Development Version 0.1-5 </strong>
		<br /><a href="https://r-forge.r-project.org/R/?group_id=58">R-Forge IPSUR Package</a>
	</div>
    
	<p>
		</p>

	</blockquote>
	<div class="articleTitle">
              <strong>Installing IPSUR from a Binary Package</strong>
            </div>
	<blockquote>
<p>
		To install the <span class="name">IPSUR</span> binary package, click on the appropriate link above to download the file. Save the package to a convenient location. 
	<p>	
		In Windows:
		<ol type="1">
			<li>
				Open <span class="name">R</span>
			</li>
			<li>
				Select <span class="name">Packages</span>
			</li>
			<li>
				Select <span class="name">Install package(s) from local zip files...</span>
			</li>
			<li>
				Browse and select the downloaded <span class="name">IPSUR</span> package
			</li>
		</ol>
		In MacOS X:
		<ol type="1">
			<li>
				Open <span class="name">R</span>
                  </li>
			<li>
				Select <span class="name">Packages &amp; data</span>
                  </li>
			<li>
				Select <span class="name">Package Installer</span>
                  </li>
			<li>
				From the Package Repository drop down menu select <span class="name">Local Binary Package</span>
                  </li>
			<li>
				Click the <span class="name">Install</span> button below
                  </li>
			<li>
				Browse and select the downloaded <span class="name">IPSUR</span> package
                  </li>
		</ol>
	</p>
</p>
</blockquote>
	
<div class="articleTitle">
              <strong>Installing IPSUR from Source</strong>
            </div>
<blockquote>
<p>
	The source code for the <span class="name">RcmdrPlugin.IPSUR</span> package is available under the GNU GPL Version 2.  The source code is recommended for platforms for which package binaries are not made available. It can also be used by developers on all platforms to modify <span class="name">IPSUR</span> and redistribute it under the constraints of the GPL. 
</p>
<p>
	Note that in order to install from source, you must have the necessary development tools installed.  In Windows, a good way to get started would be to look at the <span class="name">R Tools</span> page maintained by Duncan Murdoch, which you can find <a href="http://www.murdoch-sutherland.com/Rtools/index.html">here</a>.  Under Ubuntu Linux, this means that you should have the <span class="name">r-base-dev</span> package installed, which you can get with the Synaptic Package Manager. For more information, please see the <a href="http://cran.r-project.org/doc/manuals/R-admin.html">R Installation and Administration Manual</a>.
</p>	
	
<p>For all OSes, the general idea to install from source is to open a terminal window, then <span class="name">cd</span> to the directory in which the source code is saved, and finally issue the command <span class="name">R CMD INSTALL RcmdrPlugin.IPSUR_x.x-x</span>, where x.x-x is the version that you have downloaded.  Given that you have the correct compilation tools installed and that your system is configured correctly, you should have the package installed in a few moments. 
</p>
</p>
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

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
<title>IPSUR Project - Installation</title>
<link rel="Shortcut Icon" href="favicon.ico">
<style type="text/css">
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
    <td align="left" valign="top" bgcolor="#FFFFFF"><table width="700" height="140" border="0" cellpadding="0" cellspacing="0">
      <tr>
        <td width="200" align="center" valign="middle"><a href="index.php"><img border="0" src="resources/ipsur(main).jpg" alt="IPSUR" width="200" height="100" /></a></td>
        <td width="500" height="120" align="center" valign="middle">
          <p><a href="index.php"><img src="resources/title(main).jpg" alt="Introduction to Probability and Statistics Using R" width="500" height="100" border="0" /></a>G. Jay Kerns, Youngstown State University <br />Dr. G. Andy Chang, Youngstown State University</p></td>
      </tr>
    </table>
	<?php
		include "menu.php";
	?>
	
      <p class="articleTitle">Quick  Download and Installation Instructions</p>
      <blockquote>
        <div id="list">
			<ol type="1">
				<li><strong>Download the latest version of  <span class="name">R</span></strong>: click the link below to  download the latest version of R for your operating system from CRAN:
			  <div align="center"><br />
			  Windows - 
			  <a href="http://cran.r-project.org/bin/windows/base/">http://cran.r-project.org/bin/windows/base/</a><br />
			  MacOS X - <a href="http://cran.r-project.org/bin/macosx/">http://cran.r-project.org/bin/macosx/</a><br />
			  Linux - <a href="http://cran.r-project.org/bin/linux/">http://cran.r-project/bin/linux</a>
			  </div>
				
				<ul><br />
				  <li><strong>Windows Installation Tip for <span class="name">R</span></strong>:  click the .exe program file to start installation.  When it asks for "Customized startup options", specify Yes.  In the next 
					window, be sure to select the SDI (single-window) option.</li>
				</ul></li>
					<li><strong>Installing the <span class="name">IPSUR</span> package</strong>:
				there are several methods for installing the IPSUR package.<br /><br />
				<ul>
					<li><strong>Install from CRAN:</strong> This method works well with Windows and MaxOS X installations and insures that you have the latest stable version of the package.  To install directly from CRAN, launch R and type the following at the command prompt ">":<br />
					  <br />
					<div align="center"><br />
			      <span class="name">install.packages("IPSUR", repos="http://cran.r-project.org", dep=T)</span></div>
					</li>
					<li><strong>Install from download:</strong> Downloads of the binary packages are available on this site for Windows and MacOS X depending on the version. The packages, along with specific instructions for installing the binary packages on various platforms are available on the <a href="downloads.php">downloads page</a>. </li>
					<li><strong>Install from package source:</strong> This method of installation is recommended for all linux operating systems and all other non-supported systems. To install the current release version of the source,  type the following at the command prompt &quot;&gt;&quot;:
					  <div class="name" align="center"><br />
					    install.packages(&quot;IPSUR&quot;, contriburl = &quot;http://www.cc.ysu.edu/~gjkerns/IPSUR/package&quot;, type=&quot;source&quot;)</div>
						<p>
(<strong>note</strong>: you must have the correct compilation tools installed) </p><p>Instructions to install from a downloaded copy of the IPSUR source will be coming soon.</p>
				</ul>
			  <li><strong>
				Loading the IPSUR package:				</strong>Once the IPSUR package is downloaded and installed, it must loaded into R. To do this type the following at the command prompt &quot;&gt;&quot;: 
				<div class="name" align="center"><br />
				  library(IPSUR)</div>
			  </li>
			  
			  <li><strong>Loading the Dependencies:</strong> 
		    When <span class="name">IPSUR</span> loads it will ask you to download a bunch of additional packages. Once this procedure is finished, you will be almost ready to go.  For more detailed instructions on installing and configuring R and IPSUR you should next consult the<a href="pdf/Install.pdf"> Installing R and IPSUR</a> document. 
		    </ol>
		</div>
		</p>

				
				
				
		
		</p>
        <p>&nbsp;</p>
        </blockquote>
		 <p align="center">
	  (C) R Foundation, from http://www.r-project.org
	  <p align="center">
</td>
  </tr>
</table>

</div>
</body>
</html>

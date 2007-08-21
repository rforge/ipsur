<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
<title>IPSUR Project - What's New</title>
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
	
      <p class="articleTitle">What's New In the IPSUR Package </p>
      <blockquote>
        <p>This document collects new features that are planned for the development version of IPSUR</p>
        <p>Version </p>
		<div id="list">
		<ul>
			<li>0.1-3:
				<ul>
					<li>Features
						<ol type="1">
							<li>Plots of quantile functions</li>
							<li>Optimize simulations routines</li>
							<li>Implement distr, distrEx, packages</li>
							<li>More data</li>
						</ol>
					</li>
				</ul>
			</li>
			<li>0.1-2
				<ul>
					<li>Features
						<ol type="1">
							<li>Updated to Rcmdr 1.2-6</li>
							<li>Added suggested packages distr, distrEx</li>
							<li>Updated and completed BloodPressure data</li>
						</ol>
					</li>
					<li>Bugs
						<ol type="1">
							<li>influence plot bug fixed</li>
						</ol>
					</li>
				</ul>
			</li>
			<li>0.1-1:
				<ul>
					<li>Features
						<ol type="1">
							<li>Pareto diagrams have option rainbow / nocolor</li>
							<li>Pareto diagrams print statistics in window</li>
							<li>Simulate... menu fields now labeled to distinguish from Sampling Dist...</li>
							<li>New additions to Bar Plot
								<ol type="i">
									<li>Variable(s) in Active Data Set...
									<ol type="a">
										<li>Plot Relative Frequencies option</li>
										<li>Rainbow color option</li>
										<li>Plot Ledgen option</li>
										<li>Plot by Groups</li>
										<li>Segmented bars or Side-by-side</li>
									</ol>
									<li>Enter table... with all above options</li>
								</ol>
							</li>
					<li>New additions to Proportions menu:
						<ol type="i">
							<li>Test for equality of several proportions...</li>
							<li>Enter table for single sample...</li>
							<li>Enter table for independent samples...</li>
						</ol>
					</li>
					<li>RcmdrTestDrive was updated</li>
					<li>Plot F distribution... has now ncp parameter for density function (in R-2.4.0)</li>
						</ol>
					</li>
					<li>Bugs
						<ol type="1">
							<li>gammasimNumber bug fixed</li>
							<li>Simulate discrete uniform variates bug fixed (concerning adding to Active data set).</li>
							<li>UpdatedisunifsimNumber bug fixed</li>
							<li>Sampling Distributions => Discrete Uniform was mislabeled Cauchy</li>
							<li>Loading required packages at startup bug fixed</li>
							<li>Empty fields bug in Probability menu fixed</li>
						</ol>
					</li>
				</ul>
			</li>
			<li>0.1-0: IPSUR is introduced. See <a href="features.php">FEATURES</a>
		</ul>
		</div>
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

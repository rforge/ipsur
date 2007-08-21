<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />
<title>IPSUR Project - Features</title>
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
	
      <p class="articleTitle">Features of the IPSUR Package:</p>
	  <blockquote>
	  <p>This document describes selected features of the IPSUR package. There are three (3) sections.</p>
	  
	  <div id="list">
	  <ol type="I">
	  	<li>THE R COMMANDER: describes how the IPSUR implementation of the R Commander differs from the standard installation.</li>
      	<li>DATA: describes certain datasets provided by the package.</li>
		<li>DOCUMENTS: describes documents provided by the package.</li>
		</ol>
	  
<ol type="I">
	<li>THE R COMMANDER
		<ol type="A">
			<li>The Commander
				<ul>
					<li>An IPSUR-Probability menu is added.</li>
					<li>Distributions menu is omitted.</li>
					<li>Additional packages are recommended at startup.</li>
				</ul>
			</li>
			<li>Statistics Menu
				<ul>
					<li>Summaries, Frequency Distribution
						<ol type="i">
							<li>Allows numerical variables that are not necessarily factors to be tabularized, with a corresponding modification to the Chi-square goodness-of-fit 	
								test.</li>
							<li>Calculates relative frequencies instead of percentages.</li>
						</ol>
					</li>
					<li>Summaries, Contingency Tables, Two-Way Table
						<ol type="i">
							<li>Option &quot;Add Marginal Distributions&quot; is available which adds column and row sums to table.</li>
							<li>Hypothesis Tests has additional &quot;Simulate p-value&quot; option with customizable number of Iterations.</li>
						</ol>
					</li>
					<li>Power Menu: based on functions written by Peter Dalgaard
						<ol type="i">
							<li>Power calculation dialog for one sample, two sample, and paired t tests.</li>
							<li>Power calculation dialog for two sample proportion test.</li>
							<li>Power calculation dialog for balanced ANOVA.</li>
						</ol>
					</li>
				</ul>
			</li>
			<li>Graphs Menu
				<ul>
					<li>A Title field was added to the following plots:<br />
          				Index Plot, Histogram, Boxplot, Quantile-comparison plot, Scatterplot, Scatterplot matrix, Line Plot, Bar Graph, XY Conditioning plot.</li>
					<li>Boxplot has additional options &ldquo;Horizontal&rdquo;, &quot;Notches&quot; and &quot;Variable box width&quot;, and Horizontal, Variable box width 
						are the default settings.</li>
					<li>Pareto Diagrams were added.</li>
					<li>Strip charts were added.</li>
				</ul>
			</li>
			<li>IPSUR-Probability Menu
				<ul>
					<li>Birthday Problem dialog menu available corresponding to the stats functions pbirthday() and qbirthday(). These two functions were modified and named 
						pbirthday.ipsur() and qbirthday.ipsur() to give the precise answer (instead of asymptotics) in the case of exactly two coincidences.</li>
					<li>All parameter fields for each distribution are modified so that mathematical expressions may be entered, instead of the default restriction that only numbers
						may be entered into fields. For this reason, we may enter p=1/3 in the binomial menu or perhaps sd=7/sqrt(31) in a standard deviation field.</li>
					<li>Each distribution has a Simulate variates... option. Simulations are generated &quot;transposed&quot; from the standard Commander, with rows as 
						observations and columns as samples. The name of each generated column by default takes the form &quot;model.sim#&quot;, for example, binom.sim1, 
						binom.sim2, etc. The columns may be stored in the Active Data Set or in a new dataset (of customizable length) by default named &quot;Simset&quot;. All 
						parameters for the respective distributions are available. To generate multiple columns, a &quot;Number of samples&quot; field is available.</li>
					<li>The discrete uniform distribution is available for simulation.</li>
					<li>Noncentrality parameter fields added to Beta, Chisquared, F, and Student's t distribution.</li>
					<li>A Sampling Distributions menu is added. This is similar to (and indeed was wholly inspired by) the Distributions menu in R Commander, with cosmetic 
						differences to the interface. Rows are samples and columns are observations. Noncentrality parameters are available.  A difference is that one may calculate 
						up to three (3) sample statistics of choice from the simulated values, which can be later analyzed with the R Commander. A &quot;discard original 
						observations&quot; option is available in case the number of simulated values is inconveniently large.</li>
					<li>Discrete CDFs are plotted as right-continuous step functions.</li>
				</ul>
			</li>
		</ol>
	</li>
	<li>DATA
		<ol type="A">
			<li>RcmdrTestDrive: These are simulated data specifically designed to allow the inexperienced user to browse the capabilities of the R Commander. The R Commander has 
				extensive functionality, but many options are unavailable unless the correct types of data are loaded in the Active Data Set. This data set was randomly generated so 							
				that, when loaded, essentially all R Commander options would be available for the student to investigate. These data are entirely fictional.</li>
			<li>FeedingTimes: Data were collected concerning the feeding times of a recent newborn, Anna Lu Kerns. Variables include age in days, clock time, type of food, the 
				length of feeding time (min), and amount of food (oz). (Only beginning of dataset so far.)</li>
			<li>BloodPressure: Data were collected from 2004 through 2006 from Taoying Bian concerning  blood pressure and heart rate. Variables include date, time, systolic and 
				diastolic blood pressure, and heart rate.</li>
		</ol>
	</li>
	<li>DOCUMENTS
		<ol type="A">
			<li>&quot;Murder Madness in Toon Town&quot;. An amusing tale written to relate the fictional variables in the RcmdrTestDrive dataset. An original story contributed by 
				Jeff Cornfield in Summer 2006.</li>
		</ol>
	</li>
</ol>
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

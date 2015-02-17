<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

 <xsl:template match="/">
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="poem">
  <html>
   <head>
     <title><xsl:value-of select="@title"/></title>
   </head>
   <body>
    <h1><xsl:value-of select="@title"/></h1>
    <xsl:apply-templates/>
    <i><xsl:value-of select="@poet"/></i>
   </body>
  </html>
 </xsl:template>

 <xsl:template match="stanza">
  <p>
   <xsl:apply-templates/>
  </p>
 </xsl:template>

 <xsl:template match="stanza/line">
  <xsl:apply-templates/><br/>
 </xsl:template>

</xsl:stylesheet>

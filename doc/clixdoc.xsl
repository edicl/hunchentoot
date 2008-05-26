<?xml version="1.0" encoding="iso-8859-1" ?>

<xsl:stylesheet
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   version="1.0"
   xmlns:clix="http://bknr.net/clixdoc"
   exclude-result-prefixes="clix">

  <xsl:output method="html"
              indent="yes"
              omit-xml-declaration="yes"
              doctype-public="-//W3C//DTD HTML 4.0 Strict//EN" />

  <xsl:template match="/clix:documentation">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
        <title><xsl:value-of select="clix:title"/></title>
        <meta name="description"><xsl:attribute name="content"><xsl:value-of select="clix:short-description"/></xsl:attribute></meta>
        <style type="text/css">
  body { background-color: #ffffff }
  pre { padding:5px; background-color:#e0e0e0 }
  h3, h4 { text-decoration: underline; }
  a { text-decoration: none; padding: 1px 2px 1px 2px; }
  a:visited { text-decoration: none; padding: 1px 2px 1px 2px; }
  a:hover { text-decoration: none; padding: 1px 1px 1px 1px; border: 1px solid #000000; } 
  a:focus { text-decoration: none; padding: 1px 2px 1px 2px; border: none; }
  a.none { text-decoration: none; padding: 0; }
  a.none:visited { text-decoration: none; padding: 0; } 
  a.none:hover { text-decoration: none; border: none; padding: 0; } 
  a.none:focus { text-decoration: none; border: none; padding: 0; } 
  a.noborder { text-decoration: none; padding: 0; } 
  a.noborder:visited { text-decoration: none; padding: 0; } 
  a.noborder:hover { text-decoration: none; border: none; padding: 0; } 
  a.noborder:focus { text-decoration: none; border: none; padding: 0; }  
  pre.none { padding:5px; background-color:#ffffff }
        </style>
      </head>
      <body>
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="clix:title"/>
  <xsl:template match="clix:short-description"/>

  <xsl:template match="clix:function">
    <p>
      <xsl:choose>
        <xsl:when test="@generic = 'true'">[Generic function]</xsl:when>
        <xsl:when test="@macro = 'true'">[Macro]</xsl:when>
        <xsl:otherwise>[Function]</xsl:otherwise>
      </xsl:choose>
      <br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
        <xsl:value-of select="' '"/>
        <i><xsl:apply-templates select="clix:lambda-list"/></i>
        <xsl:if test="clix:returns">
          =&gt;
          <i><xsl:value-of select="clix:returns"/></i>
        </xsl:if>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:reader">
    <p>
      [Reader]<br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
        <xsl:value-of select="' '"/>
        <i><xsl:value-of select="@class"/></i>
        =&gt;
        <i><xsl:value-of select="clix:returns"/></i>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:accessor">
    <p>
      [Accessor]<br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
        <xsl:value-of select="' '"/>
        <i><xsl:apply-templates select="clix:lambda-list"/></i>
        =&gt;
        <i><xsl:value-of select="clix:returns"/></i>
        <br/>
        <tt>(setf (</tt><b><xsl:value-of select="@name"/></b>
        <xsl:value-of select="' '"/>
        <i><xsl:apply-templates select="clix:lambda-list"/></i><tt>) <i>new-value</i>)</tt>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:special-variable">
    <p>
      [Special variable]<br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:class">
    <p>
      [Standard class]<br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:symbol">
    <p>
      [Symbol]<br/>
      <a class="none">
        <xsl:attribute name="name">
          <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <b><xsl:value-of select="@name"/></b>
      </a>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:constant">
    <a class="none">
      <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
      <b><xsl:value-of select="@name"/></b>
    </a>
    <br/>
  </xsl:template>

  <xsl:template match="clix:constants">
    <!-- Display a list of constants with a common description -->
    <p>
      [Constants]<br/>
      <xsl:apply-templates select="clix:constant"/>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:lkw">
    <!-- lambda list keyword -->
    <tt>&#38;<xsl:value-of select="text()"/></tt>
  </xsl:template>

  <xsl:template match="clix:arg">
    <!-- argument reference -->
    <code><i><xsl:value-of select="text()"/></i></code>
  </xsl:template>

  <xsl:template name="internal-reference">
    <!-- internal reference -->
    <xsl:param name="name"/>
    <code>
      <a>
        <xsl:attribute name="href">
          #<xsl:value-of select="translate($name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <xsl:value-of select="$name"/>
      </a>
    </code>
  </xsl:template>

  <xsl:template match="clix:ref">
    <xsl:call-template name="internal-reference">
      <xsl:with-param name="name"><xsl:value-of select="."/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="clix:chapter">
    <h3>
      <a>
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
        <xsl:value-of select="@title"/>
      </a>
    </h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="clix:subchapter">
    <h4>
      <a>
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
        <xsl:value-of select="@title"/>
      </a>
    </h4>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="clix:contents">
    <ol>
      <xsl:for-each select="//clix:chapter">
        <li>
          <a>
            <xsl:attribute name="href">#<xsl:value-of select="@name"/></xsl:attribute>
            <xsl:value-of select="@title"/>
          </a>
          <xsl:if test="clix:subchapter">
            <ol>
              <xsl:for-each select="clix:subchapter">
                <li>
                  <a>
                    <xsl:attribute name="href">#<xsl:value-of select="@name"/></xsl:attribute>
                    <xsl:value-of select="@title"/>
                  </a>
                </li>
              </xsl:for-each>
            </ol>
          </xsl:if>
        </li>
      </xsl:for-each>
    </ol>
  </xsl:template>

  <xsl:template match="clix:index">
    <ul>
      <xsl:for-each select="(//clix:function | //clix:reader | //clix:accessor | //clix:class | //clix:constant | //clix:special-variable | //clix:symbol)">
        <xsl:sort select="@name"/>
        <li>
          <xsl:call-template name="internal-reference">
            <xsl:with-param name="name"><xsl:value-of select="@name"/></xsl:with-param>
          </xsl:call-template>
        </li>
      </xsl:for-each>
    </ul>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*[not(namespace-uri())]"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>

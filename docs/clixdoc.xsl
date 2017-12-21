<?xml version="1.0" encoding="iso-8859-1" ?>

<!--
;;; Copyright (c) 2008, Hans Hübner.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-->

<xsl:stylesheet
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   version="1.0"
   xmlns:clix="http://bknr.net/clixdoc"
   exclude-result-prefixes="clix">

  <xsl:output method="html"
              indent="yes"
              omit-xml-declaration="yes"
              doctype-public="-//W3C//DTD HTML 4.0 Strict//EN" />

  <xsl:key name="index-entries" match="clix:*[@name and (name() != 'clix:chapter') and (name() != 'clix:subchapter')]" use="@name" />

  <xsl:template match="/clix:documentation">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
        <title><xsl:value-of select="clix:title"/></title>
        <meta name="description"><xsl:attribute name="content"><xsl:value-of select="clix:short-description"/></xsl:attribute></meta>
        <style type="text/css">
  body { background-color: #ffffff }
  pre { padding:5px; background-color:#e0e0e0 }
  pre.none { padding:5px; background-color:#ffffff }
  h3, h4, h5 { text-decoration: underline; }
  .entry-type { padding-left: 1em; font-size: 60%; font-style: italic }
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
        </style>
      </head>
      <body>
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="clix:library-version">
    <xsl:value-of select="$library-version"/>
  </xsl:template>

  <xsl:template match="clix:title"/>
  <xsl:template match="clix:short-description"/>

  <xsl:template match="clix:function">
    <p>
      <xsl:call-template name="make-anchor"/>
      <xsl:choose>
        <xsl:when test="clix:special-definition">
          <xsl:apply-templates select="clix:special-definition"/>
        </xsl:when>
        <xsl:otherwise>
          [<xsl:call-template name="nice-entry-type-name"/>]
          <br/>
          <xsl:call-template name="render-title"/>
          <xsl:value-of select="' '"/>
          <i><xsl:apply-templates select="clix:lambda-list"/></i>
          <xsl:if test="clix:returns">
            =&gt;
            <i><xsl:apply-templates select="clix:returns"/></i>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:reader">
    <p>
      <xsl:call-template name="make-anchor"/>
      [<xsl:call-template name="nice-entry-type-name"/>]
      <br/>
      <xsl:call-template name="render-title"/>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i>
      <xsl:if test="clix:returns">
        =&gt;
        <i><xsl:apply-templates select="clix:returns"/></i>
      </xsl:if>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:listed-reader">
      <xsl:call-template name="make-anchor"/>
      <xsl:call-template name="render-title"/>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i>
      <xsl:if test="clix:returns">
        =&gt;
        <i><xsl:apply-templates select="clix:returns"/></i>
      </xsl:if>
    <br/>
  </xsl:template>

  <xsl:template match="clix:writer">
    <p>
      <xsl:call-template name="make-anchor"/>
      [<xsl:call-template name="nice-entry-type-name"/>]
      <br/>
      <tt>(setf (</tt><b><xsl:value-of select="@name"/></b>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i><tt>) <i>new-value</i>)</tt>
      <xsl:if test="clix:returns">
        =&gt;
        <i><xsl:apply-templates select="clix:returns"/></i>
      </xsl:if>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:accessor">
    <p>
      <xsl:call-template name="make-anchor"/>
      [<xsl:call-template name="nice-entry-type-name"/>]
      <br/>
      <xsl:call-template name="render-title"/>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i>
      =&gt;
      <i><xsl:apply-templates select="clix:returns"/></i>
      <br/>
      <tt>(setf (</tt><b><xsl:value-of select="@name"/></b>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i><tt>) <i>new-value</i>)</tt>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:listed-accessor">
      <xsl:call-template name="make-anchor"/>
      <xsl:call-template name="render-title"/>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i>
      =&gt;
      <i><xsl:apply-templates select="clix:returns"/></i>
      <br/>
      <tt>(setf (</tt><b><xsl:value-of select="@name"/></b>
      <xsl:value-of select="' '"/>
      <i><xsl:apply-templates select="clix:lambda-list"/></i><tt>) <i>new-value</i>)</tt>
    <br/>
  </xsl:template>

  <xsl:template match="clix:special-variable | clix:class | clix:condition | clix:symbol | clix:constant">
    <p>
      <xsl:call-template name="make-anchor"/>
      [<xsl:call-template name="nice-entry-type-name"/>]
      <br/>
      <xsl:call-template name="render-title"/>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:listed-constant">
    <xsl:call-template name="render-title"/>
    <br/>
  </xsl:template>

  <xsl:template match="clix:constants">
    <!-- Display a list of constants with a common description -->
    <p>
      [Constants]<br/>
      <xsl:apply-templates select="clix:listed-constant"/>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:readers">
    <!-- Display a list of readers with a common description -->
    <p>
      [<xsl:call-template name="nice-entry-type-name"/>]<br/>
      <xsl:apply-templates select="clix:listed-reader"/>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:accessors">
    <!-- Display a list of accessors with a common description -->
    <p>
      [<xsl:call-template name="nice-entry-type-name"/>]<br/>
      <xsl:apply-templates select="clix:listed-accessor"/>
      <blockquote>
        <xsl:apply-templates select="clix:description"/>
      </blockquote>
    </p>
  </xsl:template>

  <xsl:template match="clix:qualifier">
    <!-- method qualifier -->
    <tt><xsl:value-of select="text()"/></tt>
  </xsl:template>

  <xsl:template match="clix:lkw">
    <!-- lambda list keyword -->
    <tt>&#38;<xsl:value-of select="text()"/></tt>
  </xsl:template>

  <xsl:template match="clix:arg">
    <!-- argument reference -->
    <code><i><xsl:value-of select="text()"/></i></code>
  </xsl:template>

  <xsl:template match="clix:ref">
    <xsl:call-template name="internal-reference">
      <xsl:with-param name="name"><xsl:value-of select="."/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="clix:chapter">
    <h3>
      <a class="none">
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
      <xsl:for-each select="//clix:*[generate-id(.) = generate-id(key('index-entries', @name)[1])]">
        <xsl:sort select="@name"/>
        <li>
          <xsl:choose>
            <xsl:when test="count(key('index-entries', @name)) = 1">
              <xsl:call-template name="internal-reference">
                <xsl:with-param name="name"><xsl:value-of select="@name"/></xsl:with-param>
              </xsl:call-template>
              <span class="entry-type"><xsl:call-template name="nice-entry-type-name"/></span>
            </xsl:when>
            <xsl:otherwise>
              <a>
                <xsl:attribute name="name">
                  <xsl:value-of select="@name"/>
                </xsl:attribute>
              </a>
              <xsl:value-of select="@name"/>
              <ul>
                <xsl:for-each select="key('index-entries', @name)">
                  <xsl:sort select="name()"/>
                  <li>
                    <xsl:call-template name="internal-reference">
                      <xsl:with-param name="name"><xsl:call-template name="make-anchor-name"/></xsl:with-param>
                      <xsl:with-param name="title"><xsl:value-of select="@name"/></xsl:with-param>
                    </xsl:call-template>
                    <span class="entry-type"><xsl:call-template name="nice-entry-type-name"/></span>
                  </li>
                </xsl:for-each>
              </ul>
            </xsl:otherwise>
          </xsl:choose>
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

  <xsl:template name="internal-reference">
    <!-- internal reference -->
    <xsl:param name="name"/>
    <xsl:param name="title"/>
    <code>
      <a>
        <xsl:attribute name="href">
          #<xsl:value-of select="translate($name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
        </xsl:attribute>
        <xsl:choose>
          <xsl:when test="$title != ''">
            <xsl:value-of select="$title"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$name"/>
          </xsl:otherwise>
        </xsl:choose>
      </a>
    </code>
  </xsl:template>

  <xsl:template name="make-anchor-name">
    <xsl:choose>
      <xsl:when test="count(key('index-entries', @name)) = 1">
        <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>-<xsl:value-of select="substring(name(), 6)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="make-anchor">
    <a class="none">
      <xsl:attribute name="name">
        <xsl:call-template name="make-anchor-name"/>
      </xsl:attribute>
    </a>
  </xsl:template>

  <xsl:template name="render-title">
    <b><xsl:value-of select="@name"/></b>
  </xsl:template>

  <xsl:template name="nice-entry-type-name">
    <xsl:choose>
      <xsl:when test="name() = 'clix:function'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic function</xsl:when>
          <xsl:when test="@specialized = 'true'">Method</xsl:when>
          <xsl:when test="@macro = 'true'">Macro</xsl:when>
          <xsl:otherwise>Function</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:reader'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic reader</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized reader</xsl:when>
          <xsl:otherwise>Reader</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:listed-reader'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic reader</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized reader</xsl:when>
          <xsl:otherwise>Reader</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:readers'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic readers</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized readers</xsl:when>
          <xsl:otherwise>Readers</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:accessors'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic accessors</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized accessors</xsl:when>
          <xsl:otherwise>Accessors</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:writer'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic writer</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized writer</xsl:when>
          <xsl:otherwise>Writer</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:listed-accessor'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic accessor</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized accessor</xsl:when>
          <xsl:otherwise>Accessor</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:accessor'">
        <xsl:choose>
          <xsl:when test="@generic = 'true'">Generic accessor</xsl:when>
          <xsl:when test="@specialized = 'true'">Specialized accessor</xsl:when>
          <xsl:otherwise>Accessor</xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="name() = 'clix:special-variable'">Special variable</xsl:when>
      <xsl:when test="name() = 'clix:class'">Standard class</xsl:when>
      <xsl:when test="name() = 'clix:condition'">Condition type</xsl:when>
      <xsl:when test="name() = 'clix:symbol'">Symbol</xsl:when>
      <xsl:when test="name() = 'clix:constant'">Constant</xsl:when>
      <xsl:when test="name() = 'clix:listed-constant'">Constant</xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="name()" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

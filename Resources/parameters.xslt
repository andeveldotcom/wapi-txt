<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Hlavní šablona pro kořenový element -->
  <xsl:template match="/arguments">
    <xsl:copy>
      <xsl:choose>
        <xsl:when test=".//command-create | .//command-delete">
          <command-group>
            <!-- Zde aplikujeme šablony pouze na přímé potomky -->
            <xsl:apply-templates select="*"/>
          </command-group>
        </xsl:when>
        <xsl:otherwise>
          <option-group>
            <!-- Stejně jako výše, aplikujeme šablony pouze na přímé potomky -->
            <xsl:apply-templates select="*"/>
          </option-group>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>

  <!-- Šablona pro kopírování všech elementů -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>

  <!-- Šablona pro zpracování 'option-set' a 'option-config' -->
  <xsl:template match="option-set | option-config">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="following-sibling::*[1][self::arg]">
        <arg>
          <xsl:apply-templates select="following-sibling::*[1][self::arg]/node()" />
        </arg>
      </xsl:if>
    </xsl:copy>
    <!-- Tady jsme odstranili volání apply-templates, aby se předešlo duplicitnímu zpracování -->
  </xsl:template>

  <!-- Tato šablona se ujistí, že 'arg' není zpracována, pokud již byla zpracována výše -->
  <xsl:template match="arg[preceding-sibling::*[1][self::option-set | self::option-config]]"/>

</xsl:stylesheet>

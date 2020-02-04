package org.jetbrains.plugins.scala.util.runners;


import com.intellij.pom.java.LanguageLevel;

// required at compile time to use in annotations
public enum TestJdkVersion {

    JDK_1_8, JDK_11;

    public LanguageLevel toProductionVersion() {
        switch (this) {
            case JDK_1_8: return LanguageLevel.JDK_1_8;
            case JDK_11: return LanguageLevel.JDK_11;
            default: return null; // unreachable code
        }
    };
}

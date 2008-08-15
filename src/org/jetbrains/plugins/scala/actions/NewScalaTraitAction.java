package org.jetbrains.plugins.scala.actions;

import org.jetbrains.plugins.scala.ScalaBundle;
import org.jetbrains.plugins.scala.lang.psi.ScalaFile;
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition;
import org.jetbrains.plugins.scala.icons.Icons;
import org.jetbrains.annotations.NotNull;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiClass;

/**
 * @author ilyas
 */
public class NewScalaTraitAction extends NewScalaActionBase {

  public NewScalaTraitAction() {
    super(ScalaBundle.message("newtrait.menu.action.text"),
        ScalaBundle.message("newtrait.menu.action.description"),
        Icons.TRAIT);
  }

  protected String getActionName(PsiDirectory directory, String newName) {
    return ScalaBundle.message("newtrait.menu.action.text");
  }

  protected String getDialogPrompt() {
    return ScalaBundle.message("newtrait.dlg.prompt");
  }

  protected String getDialogTitle() {
    return ScalaBundle.message("newtrait.dlg.title");
  }

  protected String getCommandName() {
    return ScalaBundle.message("newtrait.command.name");
  }

  @NotNull
  protected PsiElement[] doCreate(String newName, PsiDirectory directory) throws Exception {
    PsiFile file = createClassFromTemplate(directory, newName, "ScalaTrait.scala");
    if (file instanceof ScalaFile) {
      ScalaFile ScalaFile = (ScalaFile) file;
      PsiClass[] classes = ScalaFile.getClasses();
      if (classes.length == 1 && classes[0] instanceof ScTypeDefinition) {
        ScTypeDefinition definition = (ScTypeDefinition) classes[0];
        PsiElement eBlock = definition.extendsBlock();
        return eBlock != null ? new PsiElement[]{definition, eBlock} : new PsiElement[]{definition};
      }
    }
    return new PsiElement[]{file};
  }
}
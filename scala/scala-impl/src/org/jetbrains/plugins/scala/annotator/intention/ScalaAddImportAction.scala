package org.jetbrains.plugins.scala.annotator.intention

import com.intellij.codeInsight.FileModificationService
import com.intellij.codeInsight.daemon.impl.actions.AddImportAction
import com.intellij.codeInsight.hint.QuestionAction
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.util.BaseListPopupStep
import com.intellij.openapi.ui.popup.{JBPopupFactory, PopupStep}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.intellij.util.ObjectUtils
import javax.swing.Icon
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScPackaging
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createDocLinkValue
import org.jetbrains.plugins.scala.lang.scaladoc.psi.api.ScDocResolvableCodeReference

abstract class ScalaAddImportAction[Elem <: PsiElement](editor: Editor, variants: Array[ElementToImport], place: Elem)
  extends QuestionAction {

  private implicit val project: Project = place.getProject

  protected def doAddImport(toImport: ElementToImport): Unit

  def execute: Boolean = {
    val validVariants = variants.filter(_.isValid)

    if (validVariants.isEmpty)
      return false

    PsiDocumentManager.getInstance(project).commitAllDocuments()
    if (validVariants.length == 1) {
      addImport(validVariants(0))
    }
    else showChooser(validVariants)

    true
  }

  private def showChooser(validVariants: Array[ElementToImport]) {
    val title = ElementToImport.messageByType(validVariants)(
      ScalaBundle.message("import.class.chooser.title"),
      ScalaBundle.message("import.package.chooser.title"),
      ScalaBundle.message("import.something.chooser.title")
    )
    val popup: BaseListPopupStep[ElementToImport] = new BaseListPopupStep[ElementToImport](title, validVariants: _*) {
      override def getIconFor(aValue: ElementToImport): Icon =
        aValue.element.getIcon(0)

      override def getTextFor(value: ElementToImport): String = {
        ObjectUtils.assertNotNull(value.qualifiedName)
      }

      override def isAutoSelectionEnabled: Boolean = false

      import com.intellij.openapi.ui.popup.PopupStep.FINAL_CHOICE

      override def onChosen(selectedValue: ElementToImport, finalChoice: Boolean): PopupStep[_] = {
        if (selectedValue == null) {
          return FINAL_CHOICE
        }
        if (finalChoice) {
          PsiDocumentManager.getInstance(project).commitAllDocuments()
          addImport(selectedValue)
          return FINAL_CHOICE
        }
        val qname: String = selectedValue.qualifiedName
        if (qname == null) return FINAL_CHOICE
        val toExclude: java.util.List[String] = AddImportAction.getAllExcludableStrings(qname)
        new BaseListPopupStep[String](null, toExclude) {
          override def onChosen(selectedValue: String, finalChoice: Boolean): PopupStep[_] = {
            if (finalChoice) {
              AddImportAction.excludeFromImport(project, selectedValue)
            }
            super.onChosen(selectedValue, finalChoice)
          }

          override def getTextFor(value: String): String = {
            "Exclude '" + value + "' from auto-import"
          }
        }
      }

      override def hasSubstep(selectedValue: ElementToImport): Boolean = {
        true
      }
    }
    JBPopupFactory.getInstance.createListPopup(popup).showInBestPositionFor(editor)
  }


  private def addImport(toImport: ElementToImport) {
    ApplicationManager.getApplication.invokeLater(() =>
      if (place.isValid && FileModificationService.getInstance.prepareFileForWrite(place.getContainingFile))
        executeWriteActionCommand("Add import action") {
          PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)
          if (place.isValid) {
            doAddImport(toImport)
          }
        }
    )
  }
}

object ScalaAddImportAction {

  def apply(editor: Editor, variants: Array[ElementToImport], ref: ScReference): ScalaAddImportAction[_] = {
    ref match {
      case docRef: ScDocResolvableCodeReference => new ForScalaDoc(editor, variants, docRef)
      case _                                    => new ForReference(editor, variants, ref)
    }
  }

  private class ForReference(editor: Editor, variants: Array[ElementToImport], ref: ScReference)
    extends ScalaAddImportAction[ScReference](editor, variants, ref) {

    protected def doAddImport(toImport: ElementToImport): Unit = {
      toImport match {
        case PrefixPackageToImport(pack) => ref.bindToPackage(pack, addImport = true)
        case _ => ref.bindToElement(toImport.element)
      }
    }

  }

  private class ForScalaDoc(editor: Editor, variants: Array[ElementToImport], ref: ScDocResolvableCodeReference)
    extends ScalaAddImportAction[ScDocResolvableCodeReference](editor, variants, ref) {

    //use fully qualified name instead of adding imports for scaladoc references
    protected def doAddImport(toImport: ElementToImport): Unit = {
      ref.replace(createDocLinkValue(toImport.qualifiedName)(ref.getManager))
    }
  }


  def getImportHolder(ref: PsiElement, project: Project): ScImportsHolder = {
    if (ScalaCodeStyleSettings.getInstance(project).isAddImportMostCloseToReference)
      PsiTreeUtil.getParentOfType(ref, classOf[ScImportsHolder])
    else {
      PsiTreeUtil.getParentOfType(ref, classOf[ScPackaging]) match {
        case null => ref.getContainingFile match {
          case holder: ScImportsHolder => holder
          case file =>
            throw new AssertionError(s"Holder is wrong, file text: ${file.getText}")
        }
        case packaging: ScPackaging => packaging
      }
    }
  }
}


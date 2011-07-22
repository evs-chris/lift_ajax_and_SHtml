import net.liftweb._
import http._
import util.Helpers._
import common._
import js._

import scala.xml.{Elem, NodeSeq, Text}

package code.snippet {
  case class Store(var name: String = "", var flavors: Seq[Flavor] = Nil)
  case class Flavor(var name: String = "", var color: String = "")
  
  object IceCream extends DispatchSnippet {
    def dispatch = {
      case "store" => form(new Store(flavors = Seq(Flavor("Strawberry", "Pink"))))
    }
    
    def form(store: Store) = {
      var flavors: Map[Int, Flavor] = store.flavors.foldLeft(1 -> Map[Int, Flavor]()) { (aggregate, flavor) => (aggregate._1 + 1) -> (aggregate._2 + (aggregate._1 -> flavor)) }._2
      var count = flavors.size
      
      def process() {
        store.flavors = flavors.map(_._2).toSeq
        S notice ("A store: " + store.toString)
        S redirectTo "/"
      }
      
      var template: NodeSeq = Text("")
      
      def aFlavor(i: Int, f: Flavor) = {
        (
          ".flavor [id]" #> ("flavor-" + i.toString) &
          ".flavor-name" #> SHtml.text(f.name, f.name = _) &
          ".flavor-color" #> SHtml.text(f.color, f.color = _) &
          ".flavor-remove" #> SHtml.ajaxButton(Text("Remove"), () => {
            flavors = flavors - i
            JsCmds.Replace("flavor-" + i, Text(""))
          })
        )(template)
      }
      
      def newFlavor = {
        val (i, f) = (count + 1, new Flavor)
        count = i
        flavors = flavors.updated(i, f)
        
        js.jquery.JqJsCmds.AppendHtml("flavors", aFlavor(i, f))
      }
      
      "name=store-name" #> SHtml.text(store.name, store.name = _) &
      "#new-flavor-button" #> SHtml.ajaxButton(Text("Add Flavor"), () => newFlavor) &
      "type=submit" #> S.formGroup(1000) { SHtml.onSubmitUnit(process) } &
      "#flavors *" #> { xml => template = xml; flavors.map(p => aFlavor(p._1, p._2)).toSeq.flatten }
    }
  }
}
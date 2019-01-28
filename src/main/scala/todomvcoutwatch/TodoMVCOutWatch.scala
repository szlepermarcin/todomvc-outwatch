package todomvcoutwatch

import cats.effect.IO
import cats.instances.string.catsKernelStdOrderForString
import monix.execution.Ack
import monix.execution.Scheduler.Implicits.global
import monix.reactive.{Observable, Observer}
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.Input
import outwatch.dom._
import outwatch.dom.dsl.{htmlTag, _}
import outwatch.dom.dsl.events.window
import outwatch.util.Store
import upickle.default.{ReadWriter => RW, macroRW}

object TodoMVCOutWatch {

  final case class Todo(text: String, active: Boolean, editing: Boolean)
  object Todo {
    implicit val rw: RW[Todo] = macroRW
  }

  final case class AppState(todoList: List[Todo], newTodo: String, todoFilter: Option[Boolean])

  sealed trait TodoAction
  final case class UpdateFilter(rp: String) extends TodoAction
  final case class UpdateText(value: String) extends TodoAction
  final case object ClearCompleted extends TodoAction
  final case object AddTodo extends TodoAction
  final case object ToggleAll extends TodoAction
  final case class ToggleTodo(id: Int) extends TodoAction
  final case class EditTodo(id: Int) extends TodoAction
  final case class UpdateTodo(id: Int, value: String) extends TodoAction
  final case class DeleteTodo(id: Int) extends TodoAction


  def main(args: Array[String]): Unit = {

    val hashRouter: IO[Observable[String]] = IO {
      Observable(
        window.onLoad.map(_ => ()),
        window.onHashChange.map(_ => ())
      ).merge[Unit]
        .map(_ => dom.window.location.hash.replace("#", ""))
        .distinctUntilChanged
    }

    val reducer = (state: AppState, action: TodoAction) => action match {
      case UpdateFilter(rp) =>
        rp.split("/").toList.filter(_.nonEmpty) match {
          case "active" :: _ =>
            state.copy(todoFilter = Some(true))
          case "completed" :: _ =>
            state.copy(todoFilter = Some(false))
          case _ =>
            state.copy(todoFilter = None)
        }
      case UpdateText(v) =>
        state.copy(newTodo = v)
      case ClearCompleted =>
        state.copy(todoList = state.todoList.filter(_.active))
      case AddTodo =>
        if (state.newTodo != "")
          state.copy(todoList = state.todoList :+ Todo(state.newTodo, active = true, editing = false), newTodo = "")
        else
          state
      case ToggleAll =>
        state.copy(todoList = state.todoList.map(t => t.copy(active = state.todoList.forall(!_.active))))
      case ToggleTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(active = !v._1.active) else v._1))
      case EditTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(editing = true) else v._1))
      case UpdateTodo(todoId, todoValue) =>
        if (todoValue == "") {
          state.copy(todoList = state.todoList.zipWithIndex.filterNot(_._2 == todoId).map(_._1))
        } else {
          state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(text = todoValue, editing = false) else v._1))
        }
      case DeleteTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.filterNot(_._2 == todoId).map(_._1))
      case _ =>
        state
    }

    val todoInput = (state: Observable[AppState], store: Observer[TodoAction]) => header(
      Seq("header").map(cls := _),
      input(
        Seq("new-todo").map(cls := _),
        placeholder := "What needs to be done?",
        onInput.value.map(s => UpdateText(s)) --> store,
        value <-- state.map(_.newTodo).distinctUntilChanged,
        onKeyUp.filter(t => t.keyCode == 13).map(_ => AddTodo) --> store
      )
    )

    val todoListSection = (state: AppState, store: Observer[TodoAction], focusHandler: Handler[Int]) => htmlTag("section")(
      Seq("main").map(cls := _),
      input(
        id := "toggle-all",
        cls := "toggle-all",
        tpe := "checkbox",
        checked := state.todoList.forall(!_.active),
        onChange(ToggleAll) --> store
      ),
      label(forId := "toggle-all", "Mark all as complete"),
      ul(
        cls := "todo-list",
        state.todoList.zipWithIndex
          .filter(v => {
            state.todoFilter match {
              case Some(true) => v._1.active
              case Some(false) => !v._1.active
              case _ => true
            }
          }).map(t => {
          li(
            Some(cls := "completed").filterNot(_ => t._1.active),
            Some(cls := "editing").filter(_ => t._1.editing),
            div(
              cls := "view",
              input(
                cls := "toggle",
                tpe := "checkbox",
                value := "on",
                checked := !t._1.active,
                onChange(ToggleTodo(t._2)) --> store
              ),
              label(
                t._1.text
              )
            ),
            input(
              managedElement
                .asHtml(e =>
                  focusHandler
                    .filter(_ == t._2)
                    .subscribe(_ => IO(e.focus()).map(_ => Ack.Continue).unsafeToFuture())
                ),
              cls := "edit",
              value := t._1.text,
              onKeyUp.filter(t => t.keyCode == 13)
                .map(ev => UpdateTodo(t._2, ev.target.asInstanceOf[Input].value)) --> store,
              eventProp[Event]("focusout")
                .map(ev => UpdateTodo(t._2, ev.target.asInstanceOf[Input].value)) --> store,
            ),
            button(cls := "destroy", onClick(DeleteTodo(t._2)) --> store),
            onDblClick(EditTodo(t._2)) --> store,
            onDblClick(t._2) --> focusHandler
          )
        })
      )
    )

    Observer

    val itemsLeftDesc = (cnt: Int) => s"$cnt ${if(cnt == 1) "item" else "items"} left"

    val filterElement = (txt: String, link: String, active: Boolean) =>
      li(
        a(
          if (active) Some(cls := "selected") else None,
          href := link,
          txt
        )
      )

    val todoFooter = (state: AppState, store: Observer[TodoAction]) => footer(
      cls := "footer",
      span(
        cls := "todo-count",
        itemsLeftDesc(state.todoList.count(_.active))
      ),
      ul(
        cls := "filters",
        filterElement("All","#/", state.todoFilter.isEmpty),
        filterElement("Active","#/active", state.todoFilter.contains(true)),
        filterElement("Completed","#/completed", state.todoFilter.contains(false))
      ),
      state.todoList.find(!_.active)
        .map(_ => {
          button(
            cls := "clear-completed",
            "Clear completed",
            onClick(ClearCompleted) --> store
          )
        })
    )

    val localStore =(key: String) => (data: String) => IO(dom.window.localStorage.setItem(key, data))

    val app = for {
      initial <- IO(Option(dom.window.localStorage.getItem("todomvc-outwatch")).map(s => upickle.default.read[List[Todo]](s)).getOrElse(List.empty))
      writeLocal = localStore("todomvc-outwatch")
      store <- Store.create[AppState, TodoAction](AppState(initial,"",None), reducer)
      _ = store.mapEvalF(st => writeLocal(upickle.default.write(st.todoList))).foreachL(identity).runAsync(_ => ())
      router <- hashRouter
      _ <- IO(router.map(s => UpdateFilter(s)).subscribe(store))
      focusHandler <-  Handler.create[Int]
      result = div(
        h1("todos"),
        todoInput(store, store),
        store.map(state =>
          Some(
            Seq(
              todoListSection(state, store, focusHandler),
              todoFooter(state, store)
            )
          ).filterNot(_ => state.todoList.isEmpty)
        )
      )
      _
        <- OutWatch.renderInto(".todoapp", result)
    } yield ()

    app.unsafeRunSync()
  }
}

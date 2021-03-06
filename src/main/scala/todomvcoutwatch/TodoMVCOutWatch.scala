package todomvcoutwatch

import cats.effect.IO
import cats.instances.boolean._
import cats.instances.option._
import cats.instances.string._
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

  final case class UpdateFilter(filter: Option[Boolean]) extends TodoAction

  final case class UpdateText(value: String) extends TodoAction

  final case object ClearCompleted extends TodoAction

  final case object AddTodo extends TodoAction

  final case object ToggleAll extends TodoAction

  final case class ToggleTodo(id: Int) extends TodoAction

  final case class EditTodo(id: Int) extends TodoAction

  final case class UpdateTodo(id: Int, value: String) extends TodoAction

  final case class DeleteTodo(id: Int) extends TodoAction

  val LocalStorageKey = "todomvc-outwatch"
  val EnterKey = 13

  def main(args: Array[String]): Unit = {

    val hashRouter: IO[Observable[Option[Boolean]]] = IO {
      Observable(window.onLoad.map(_ => ()), window.onHashChange.map(_ => ())).merge[Unit]
        .map(_ => dom.window.location.hash.replace("#", "").split("/").toList.filter(_.nonEmpty))
        .map {
          case "active" :: _ => Some(true)
          case "completed" :: _ => Some(false)
          case _ => None
        }
        .distinctUntilChanged
    }

    val reducer = (state: AppState, action: TodoAction) => action match {
      case UpdateFilter(filter) =>
        state.copy(todoFilter = filter)
      case UpdateText(v) =>
        state.copy(newTodo = v)
      case ClearCompleted =>
        state.copy(todoList = state.todoList.filter(_.active))
      case AddTodo if state.newTodo.nonEmpty =>
        state.copy(todoList = state.todoList :+ Todo(state.newTodo, active = true, editing = false), newTodo = "")
      case ToggleAll =>
        state.copy(todoList = state.todoList.map(t => t.copy(active = state.todoList.forall(!_.active))))
      case ToggleTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(active = !v._1.active) else v._1))
      case EditTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(editing = true) else v._1))
      case UpdateTodo(todoId, todoValue) if todoValue.nonEmpty =>
        state.copy(todoList = state.todoList.zipWithIndex.map(v => if (v._2 == todoId) v._1.copy(text = todoValue, editing = false) else v._1))
      case UpdateTodo(todoId, _) =>
        state.copy(todoList = state.todoList.zipWithIndex.filterNot(_._2 == todoId).map(_._1))
      case DeleteTodo(todoId) =>
        state.copy(todoList = state.todoList.zipWithIndex.filterNot(_._2 == todoId).map(_._1))
      case _ =>
        state
    }

    val todoInput = (state: Observable[AppState], store: Observer[TodoAction]) => header(
      cls := "header",
      input(
        Seq("new-todo").map(cls := _),
        placeholder := "What needs to be done?",
        onInput.value.map(s => UpdateText(s)) --> store,
        autoFocus := true,
        value <-- state.map(_.newTodo).distinctUntilChanged,
        onKeyUp.filter(t => t.keyCode == EnterKey).map(_ => AddTodo) --> store
      )
    )

    val todoListSection = (state: AppState, store: Observer[TodoAction], focusHandler: Handler[Int]) => htmlTag("section")(
      cls := "main",
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
          .filter { case (todo, _) =>
            state.todoFilter match {
              case Some(true) => todo.active
              case Some(false) => !todo.active
              case _ => true
            }
          }.map { case (todo, todoId) =>
          li(
            Some(cls := "completed").filterNot(_ => todo.active),
            Some(cls := "editing").filter(_ => todo.editing),
            div(
              cls := "view",
              input(
                cls := "toggle",
                tpe := "checkbox",
                value := "on",
                checked := !todo.active,
                onChange(ToggleTodo(todoId)) --> store
              ),
              label(todo.text)
            ),
            input(
              managedElement.asHtml(element =>
                focusHandler
                  .filter(_ == todoId)
                  .subscribe(_ => IO(element.focus()).map(_ => Ack.Continue).unsafeToFuture())
              ),
              cls := "edit",
              value := todo.text,
              onKeyUp.filter(t => t.keyCode == EnterKey)
                .map(ev => UpdateTodo(todoId, ev.target.asInstanceOf[Input].value)) --> store,
              eventProp[Event]("focusout")
                .map(ev => UpdateTodo(todoId, ev.target.asInstanceOf[Input].value)) --> store,
            ),
            button(cls := "destroy", onClick(DeleteTodo(todoId)) --> store),
            onDblClick(EditTodo(todoId)) --> store,
            onDblClick(todoId) --> focusHandler
          )
        }
      )
    )

    val itemsLeftDesc = (cnt: Int) => s"$cnt ${if (cnt == 1) "item" else "items"} left"

    val filterElement = (txt: String, link: String, active: Boolean) =>
      li(a(Some(cls := "selected").filter(_ => active), href := link, txt))

    val todoFooter = (state: AppState, store: Observer[TodoAction]) => footer(
      cls := "footer",
      span(cls := "todo-count", itemsLeftDesc(state.todoList.count(_.active))),
      ul(
        cls := "filters",
        filterElement("All", "#/", state.todoFilter.isEmpty),
        filterElement("Active", "#/active", state.todoFilter.contains(true)),
        filterElement("Completed", "#/completed", state.todoFilter.contains(false))
      ),
      state.todoList.find(!_.active)
        .map(_ => button(cls := "clear-completed", "Clear completed", onClick(ClearCompleted) --> store))
    )

    val localStore = (key: String) => (data: String) => IO(dom.window.localStorage.setItem(key, data))

    val app = for {
      initial <- IO(Option(dom.window.localStorage.getItem(LocalStorageKey)).map(s => upickle.default.read[List[Todo]](s)).getOrElse(List.empty))
      writeLocal = localStore(LocalStorageKey)
      store <- Store.create[AppState, TodoAction](AppState(initial, "", None), reducer)
      _ = store.mapEvalF(st => writeLocal(upickle.default.write(st.todoList))).foreachL(identity).runAsync(_ => ())
      router <- hashRouter
      _ <- IO(router.map(s => UpdateFilter(s)).subscribe(store))
      focusHandler <- Handler.create[Int]
      result = div(
        h1("todos"),
        todoInput(store, store),
        store.map(state =>
          state.todoList match {
            case Nil => Seq.empty
            case _ => Seq(todoListSection(state, store, focusHandler), todoFooter(state, store))
          }
        )
      )
      _ <- OutWatch.renderInto(".todoapp", result)
    } yield ()

    app.unsafeRunSync()
  }
}
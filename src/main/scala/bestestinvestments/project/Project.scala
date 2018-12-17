package bestestinvestments.project

import java.util.Calendar

class ClientId

class ProjectId

class ProjectManagerId

class SpecialistId

class Reference

class Minutes private(val amount: Int)

object Minutes {
  def apply(amount: Int): Option[Minutes] =
    if (amount > 0) {
      Some(new Minutes(amount))
    } else {
      None
    }
}

sealed trait Project {
  var projectIdentifier: ProjectId
}

case class DraftedProject(name: String, projectedEndDate: Calendar, client: ClientId) extends Project {
  override var projectIdentifier: ProjectId = new ProjectId

  def start(projectManager: ProjectManagerId): Either[String, ActiveProject] =
    Right(ActiveProject(this))
}

case class ActiveProject(
  override var projectIdentifier: ProjectId,
  projectManager: Option[ProjectManagerId],
  pendingSpecialists: List[SpecialistId],
  activeSpecialists: List[SpecialistId],
  consultations: Consultations
) extends Project {

  def allSpecialists: List[SpecialistId] = pendingSpecialists ::: activeSpecialists

  def addSpecialist(specialist: SpecialistId): Either[String, ActiveProject] = {
    if (hasSpecialist(specialist)) {
      Left(s"Project already contains specialist: $specialist")
    } else {
      Right(
        this.copy(pendingSpecialists = specialist :: pendingSpecialists)
      )
    }
  }

  def approveSpecialist(specialist: SpecialistId): Either[String, ActiveProject] = {
    if (!hasPendingSpecialist(specialist)) {
      Left(s"Project does not contains specialist to approve: $specialist")
    } else {
      val newPendingSpecialists = pendingSpecialists.filterNot(_ == specialist)
      Right(
        this.copy(
          pendingSpecialists = newPendingSpecialists,
          activeSpecialists = specialist :: activeSpecialists
        )
      )
    }
  }

  def discardSpecialist(specialist: SpecialistId): Either[String, ActiveProject] = {
    if (!hasPendingSpecialist(specialist)) {
      Left(s"Project does not contains specialist to discard: $specialist")
    } else {
      val newPendingSpecialists = pendingSpecialists.filterNot(_ == specialist)
      Right(
        this.copy(
          pendingSpecialists = newPendingSpecialists
        )
      )
    }
  }

  def scheduleConsultation(specialist: SpecialistId, time: Calendar): Either[String, ActiveProject] = {
    if (!hasActiveSpecialist(specialist)) {
      Left(s"Project does not contains specialist to schedule consultation: $specialist")
    } else {
      val consultation = OpenConsultation(specialist, time)
      Right(
        this.copy(
          consultations = consultations.add(consultation)
        )
      )
    }
  }

  def reportConsultation(consultationId: ConsultationId, time: Minutes): Either[String, ActiveProject] =
    for (
      newConsultations <- consultations.report(consultationId, time)
    ) yield this.copy(consultations = newConsultations)

  def discardConsultation(consultationId: ConsultationId): Either[String, ActiveProject] =
    for (
      newConsultations <- consultations.discard(consultationId)
    ) yield this.copy(consultations = newConsultations)

  def close(): Either[String, ClosedProject] =
    if (consultations.areOpen) {
      Left("Consultations are still open")
    } else {
      Right(ClosedProject(this.projectIdentifier))
    }

  private def hasPendingSpecialist(specialistId: SpecialistId) = pendingSpecialists contains specialistId

  private def hasActiveSpecialist(specialistId: SpecialistId) = activeSpecialists contains specialistId

  private def hasSpecialist(specialistId: SpecialistId) = allSpecialists contains specialistId

}

case class ClosedProject(override var projectIdentifier: ProjectId) extends Project

object Project {
  def draft(name: String, projectedEndDate: Calendar, client: ClientId): DraftedProject =
    DraftedProject(name, projectedEndDate, client)
}

object Main {

  def exampleProject = {
    val specialist = new SpecialistId
    val initialProject = Project.draft("new project", Calendar.getInstance(), new ClientId);
    for {
      opened           <- initialProject.start(new ProjectManagerId)
//      withSpecialist   <- opened.addSpecialist(specialist)
      approved         <- opened.approveSpecialist(specialist)
      withConsultation <- approved.scheduleConsultation(specialist, Calendar.getInstance())
      reported         <- withConsultation.reportConsultation(withConsultation.consultations.consultations.head.consultationId, Minutes(15).get)
      closed           <- reported.close()
    } yield closed
  }


  def main(args: Array[String]): Unit = {
    println(exampleProject)
  }
}

object ActiveProject {
  def apply(draft: DraftedProject): ActiveProject =
    ActiveProject(draft.projectIdentifier, None, List.empty, List.empty, Consultations.empty)
}
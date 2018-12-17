package bestestinvestments.project

import java.util.Calendar

class ConsultationId

sealed trait Consultation {
  val isOpen: Boolean = false
  val consultationId: ConsultationId
}

case class OpenConsultation(specialist: SpecialistId, time: Calendar) extends Consultation {
  override val isOpen = true
  val consultationId = new ConsultationId

  def report(time: Minutes) =
    ConfirmedConsultation(consultationId, time)

  def discard() = DiscardedConsultation(consultationId)
}

case class ConfirmedConsultation(override val consultationId: ConsultationId, time: Minutes) extends Consultation

case class DiscardedConsultation(override val consultationId: ConsultationId) extends Consultation

case class Consultations(consultations: List[Consultation]) {

  def areOpen: Boolean = consultations.exists(_.isOpen)

  def add(openConsultation: OpenConsultation): Consultations =
    this.copy(consultations = openConsultation :: consultations)

  def report(consultationId: ConsultationId, time: Minutes): Either[String, Consultations] =
    updated(consultationId, oc => oc.report(time))

  def discard(consultationId: ConsultationId): Either[String, Consultations] =
    updated(consultationId, oc => oc.discard())

  private def updated(consultationId: ConsultationId, update: OpenConsultation => Consultation) =
    consultations.find(_.consultationId == consultationId) match {
      case Some(currentConsultation @ OpenConsultation(_, _)) =>
        val newConsultations = consultations.map({
          case `currentConsultation` => update(currentConsultation)
          case c => c
        })
        Right(Consultations(newConsultations))
      case Some(_) =>
        Left("Consultation is not open")
      case None =>
        Left("Could not find consultation")
    }
}

object Consultations {
  def empty: Consultations = Consultations(List.empty)
}
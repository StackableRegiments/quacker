package metl.model

class SqlRepository(driver: String,
                    url: String,
                    username: String,
                    password: String)
    extends RepositoryWrapper(new InMemoryRepository) {

  override def startup = {
    super.startup
  }
  override def shutdown = {
    super.shutdown
  }
}

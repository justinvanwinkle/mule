def pytest_addoption(parser):
    parser.addoption(
        "--corpus-strict",
        action="store_true",
        help="Treat documented corpus compiler gaps as failures rather than xfails.",
    )

import { test } from "@playwright/test";
import { MainPage } from "../pages/main-page";
import { RepositoriesPage } from "../pages/repositories-page";
import { RepositoryPage } from "../pages/repository-page";

test.describe("Repository Management", () => {
  test.describe("Repository Lifecycle", () => {
    test("should add repository and navigate to repository page", async ({
      page,
    }) => {
      const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;
      const mainPage = new MainPage(page);
      const repositoriesPage = new RepositoriesPage(page);
      const repositoryPage = new RepositoryPage(page);

      await mainPage.goto();
      await mainPage.navigateToRepositories();
      await repositoriesPage.addRepository(
        repoName,
        "https://github.com/srid/haskell-template",
      );
      await repositoryPage.verifyRepositoryPage(repoName);
      await repositoryPage.deleteRepository();
      await repositoriesPage.verifyRepositoryNotListed(repoName);
    });
  });

  test.describe("Branch Management", () => {
    test("should show 0 branches initially, then populate after refresh", async ({
      page,
    }) => {
      const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;
      const mainPage = new MainPage(page);
      const repositoriesPage = new RepositoriesPage(page);
      const repositoryPage = new RepositoryPage(page);

      await mainPage.goto();
      await mainPage.navigateToRepositories();
      await repositoriesPage.addRepository(
        repoName,
        "https://github.com/srid/haskell-template",
      );
      await repositoryPage.verifyRepositoryPage(repoName);
      await repositoryPage.verifyNoBranches();
      await repositoryPage.refreshRepository();
      await repositoryPage.verifyBranchesPresent();
      await repositoryPage.verifyBranchExists("master");
    });
  });
});

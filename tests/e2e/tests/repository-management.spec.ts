import { test } from "@playwright/test";
import { MainPage } from "../pages/main-page";
import { RepositoriesPage } from "../pages/repositories-page";
import { RepositoryPage } from "../pages/repository-page";

test.describe("Repository Management", () => {
  let mainPage: MainPage;
  let repositoriesPage: RepositoriesPage;
  let repositoryPage: RepositoryPage;
  let repoName: string;

  test.beforeEach(async ({ page }) => {
    repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;
    mainPage = new MainPage(page);
    repositoriesPage = new RepositoriesPage(page);
    repositoryPage = new RepositoryPage(page);

    await mainPage.goto();
    await mainPage.navigateToRepositories();
  });

  test.describe("Repository Lifecycle", () => {
    test("should add repository and navigate to repository page", async () => {
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
    test.afterEach(async () => {
      // Clean up: delete repository after branch tests
      try {
        await repositoryPage.deleteRepository();
      } catch {
        // Repository might already be deleted, ignore errors
      }
    });

    test("should show 0 branches initially, then populate after refresh", async () => {
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

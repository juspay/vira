import { test, expect } from "@playwright/test";

test("add repository and navigate to repository page", async ({ page }) => {
  const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;

  // Navigate to repositories page
  await page.goto("/");
  await page.getByRole("link", { name: /repositories/i }).click();

  // Add new repository - this redirects to the repository page
  await page.getByLabel("Repository Name").fill(repoName);
  await page
    .getByLabel("Git Clone URL")
    .fill("https://github.com/srid/haskell-template");
  await page.getByRole("button", { name: /add repository/i }).click();

  // Verify redirect to repository page and repository name appears
  await expect(page).toHaveURL(new RegExp(`/r/${repoName}`));
  await expect(page.getByRole("heading", { name: repoName })).toBeVisible();

  // Set up dialog handler before clicking delete
  page.on("dialog", (dialog) => dialog.accept());

  // Delete the repository
  await page.getByRole("button", { name: /delete/i }).click();

  // Wait for redirect to repositories page
  await page.waitForURL(/\/r$/);

  // Verify repository is no longer listed
  await expect(page.getByRole("link", { name: repoName })).not.toBeVisible();
});

test("add repository, verify 0 branches, then refresh and verify >=1 branches", async ({
  page,
}) => {
  const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;

  // Navigate to repositories page and add repository - this redirects to repo page
  await page.goto("/");
  await page.getByRole("link", { name: /repositories/i }).click();
  await page.getByLabel("Repository Name").fill(repoName);
  await page
    .getByLabel("Git Clone URL")
    .fill("https://github.com/srid/haskell-template");
  await page.getByRole("button", { name: /add repository/i }).click();

  // Verify redirect to repository page and repository has 0 branches initially
  await expect(page).toHaveURL(new RegExp(`/r/${repoName}`));
  await expect(page.locator("#branch-count")).toContainText("0 branches");

  // Click refresh button to populate branches
  await page.getByRole("button", { name: /refresh/i }).click();

  // Wait for branches to be populated and verify we have at least 1 branch
  await expect(page.locator("#branch-count")).not.toContainText("0 branches", {
    timeout: 15000,
  });
  await expect(page.locator("#branch-count")).toHaveText(/[1-9]\d* branches?/);

  // Verify that 'master' branch is in the list
  await expect(page.getByText("master")).toBeVisible();
});

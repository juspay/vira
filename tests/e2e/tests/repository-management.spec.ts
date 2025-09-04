import { test, expect } from "@playwright/test";

test("add repository and navigate to repository page", async ({ page }) => {
  const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;

  // Navigate to repositories page
  await page.goto("/");
  await page.getByRole("link", { name: /repositories/i }).click();

  // Add new repository
  await page.getByLabel("Repository Name").fill(repoName);
  await page
    .getByLabel("Git Clone URL")
    .fill("https://github.com/srid/haskell-template");
  await page.getByRole("button", { name: /add repository/i }).click();

  // Verify repository was added
  await page.reload();
  await expect(page.getByRole("link", { name: repoName })).toBeVisible();

  // Navigate to the repository page
  await page.getByRole("link", { name: repoName }).click();

  // Verify repository name appears on the page
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

test("add repository and verify 0 branches initially", async ({ page }) => {
  const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;

  // Navigate to repositories page and add repository
  await page.goto("/");
  await page.getByRole("link", { name: /repositories/i }).click();
  await page.getByLabel("Repository Name").fill(repoName);
  await page
    .getByLabel("Git Clone URL")
    .fill("https://github.com/srid/haskell-template");
  await page.getByRole("button", { name: /add repository/i }).click();

  // Navigate to the repository page
  await page.reload();
  await page.getByRole("link", { name: repoName }).click();

  // Verify repository has 0 branches initially
  await expect(page.locator("#branch-count")).toContainText("0 branches");
});

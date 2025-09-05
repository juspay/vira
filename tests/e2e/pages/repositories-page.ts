import { Page, expect } from "@playwright/test";

export class RepositoriesPage {
  constructor(private page: Page) {}

  async addRepository(name: string, cloneUrl: string) {
    await this.page.getByLabel("Repository Name").fill(name);
    await this.page.getByLabel("Git Clone URL").fill(cloneUrl);

    // Wait for navigation after clicking add repository
    await Promise.all([
      this.page.waitForURL(new RegExp(`/r/${name}`), { timeout: 10000 }),
      this.page.getByRole("button", { name: /add repository/i }).click(),
    ]);
  }

  async verifyRepositoryNotListed(name: string) {
    await expect(this.page.getByRole("link", { name })).not.toBeVisible();
  }
}
